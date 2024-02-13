{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}

#ifdef IOS
#else
{-# LANGUAGE TemplateHaskell     #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso
  ( miso
  , startApp
  , module Miso.Effect
  , module Miso.Event
  , module Miso.Html
  , module Miso.Subscription
#ifndef ghcjs_HOST_OS
  , module Miso.TypeLevel
#endif
  , module Miso.Types
  , module Miso.Router
  , module Miso.Util
  , module Miso.FFI
  , module Miso.WebSocket
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.List
import           Data.Sequence ((|>))
import           System.Mem.StableName
import qualified Data.Sequence as S
import qualified JavaScript.Object.Internal as OI

#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle (eval, waitForAnimationFrame)
#ifdef IOS
import           Miso.JSBits
#else
import           GHCJS.Types (JSString)
import           Data.FileEmbed
#endif
#else
import           JavaScript.Web.AnimationFrame
#endif

import           Miso.Concurrent
import           Miso.Delegate
import           Miso.Diff
import           Miso.Effect
import           Miso.Event
import           Miso.FFI
import           Miso.Html
import           Miso.Router
import           Miso.Subscription
#ifndef ghcjs_HOST_OS
import           Miso.TypeLevel
#endif
import           Miso.Types
import           Miso.Util
import           Miso.WebSocket

-- | Helper function to abstract out common functionality between `startApp` and `miso`
common
  :: Eq model
  => App model action
  -> model
  -> (Sink action -> JSM (IORef VTree))
  -> JSM ()
common App {..} m getView = do
#ifndef ghcjs_HOST_OS
#ifdef IOS
  mapM_ eval [delegateJs,diffJs,isomorphicJs,utilJs]
#else
  _ <- eval ("window = typeof window === 'undefined' ? {} : window;\nwindow['oldCallbacks'] = [];\nwindow['currentCallbacks'] = [];\n\n/* Callbacks in ghcjs need to be released. With this function one can register\n   callbacks that should be released right before diffing.\n   */\nwindow['registerCallback'] = function registerCallback(cb) {\n  window['currentCallbacks'].push(cb);\n};\n\n/* Swaps out the new calbacks for old callbacks.\nThe old callbacks should be cleared once the new callbacks have replaced them.\n*/\nwindow['swapCallbacks'] = function swapCallbacks() {\n  window['oldCallbacks'] = window['currentCallbacks'];\n  window['currentCallbacks'] = [];\n};\n\n/* This releases the old callbacks. */\nwindow['releaseCallbacks'] = function releaseCallbacks() {\n  for (var i in window['oldCallbacks'])\n    h$release(window['oldCallbacks'][i]);\n\n  window['oldCallbacks'] = [];\n};\n\n/* event delegation algorithm */\nwindow['delegate'] = function delegate(mountPointElement, events, getVTree) {\n  for (var event in events) {\n    mountPointElement.addEventListener(events[event][0], function(e) {\n      getVTree(function (obj) {\n        window['delegateEvent'](e, obj, window['buildTargetToElement'](mountPointElement, e.target), []);\n      });\n    }, events[event][1]);\n  }\n};\n\n/* Accumulate parent stack as well for propagation */\nwindow['delegateEvent'] = function delegateEvent (event, obj, stack, parentStack) {\n\n  /* base case, not found */\n  if (!stack.length) return;\n\n  /* stack not length 1, recurse */\n  else if (stack.length > 1) {\n    parentStack.unshift(obj);\n    for (var o = 0; o < obj.children.length; o++) {\n      if (obj.children[o]['domRef'] === stack[1]) {\n        delegateEvent( event, obj.children[o], stack.slice(1), parentStack );\n        break;\n      }\n    }\n  }\n\n  /* stack.length == 1 */\n  else {\n    var eventObj = obj['events'][event.type];\n    if (eventObj) {\n      var options = eventObj.options;\n      if (options['preventDefault'])\n        event.preventDefault();\n      eventObj['runEvent'](event);\n      if (!options['stopPropagation'])\n        window['propogateWhileAble'] (parentStack, event);\n    } else {\n      /* still propagate to parent handlers even if event not defined */\n      window['propogateWhileAble'] (parentStack, event);\n    }\n  }\n};\n\nwindow['buildTargetToElement'] = function buildTargetToElement (element, target) {\n  var stack = [];\n  while (element !== target) {\n    stack.unshift (target);\n    target = target.parentNode;\n  }\n  return stack;\n};\n\nwindow['propogateWhileAble'] = function propogateWhileAble (parentStack, event) {\n  for (var i = 0; i < parentStack.length; i++) {\n    if (parentStack[i]['events'][event.type]) {\n      var eventObj = parentStack[i]['events'][event.type],\n        options = eventObj['options'];\n      if (options['preventDefault']) event.preventDefault();\n      eventObj['runEvent'](event);\n      if (options['stopPropagation']) break;\n    }\n  }\n};\n\n/* Walks down obj following the path described by `at`, then filters primitive\n values (string, numbers and booleans)*/\nwindow['objectToJSON'] = function objectToJSON (at, obj) {\n  /* If at is of type [[MisoString]] */\n  if (typeof at[0] == 'object') {\n    var ret = [];\n    for (var i = 0; i < at.length; i++)\n      ret.push(window['objectToJSON'](at[i], obj));\n    return ret;\n  }\n\n  for (var i in at) obj = obj[at[i]];\n\n  /* If obj is a list-like object */\n  var newObj;\n  if (obj instanceof Array || ('length' in obj && obj['localName'] !== 'select')) {\n    newObj = [];\n    for (var i = 0; i < obj.length; i++)\n      newObj.push(window['objectToJSON']([], obj[i]));\n    return newObj;\n  }\n\n  /* If obj is a non-list-like object */\n  newObj = {};\n  for (var i in getAllPropertyNames(obj)){\n    /* bug in safari, throws TypeError if the following fields are referenced on a checkbox */\n    /* https://stackoverflow.com/a/25569117/453261 */\n    /* https://html.spec.whatwg.org/multipage/input.html#do-not-apply */\n    if ((obj['localName'] === 'input') && (i === 'selectionDirection' || i === 'selectionStart' || i === 'selectionEnd'))\n      continue;\n    if (typeof obj[i] == 'string' || typeof obj[i] == 'number' || typeof obj[i] == 'boolean')\n      newObj[i] = obj[i];\n  }\n  return newObj;\n};\n\n/* get static and dynamic properties */\nfunction getAllPropertyNames(obj) {\n  var props = {}, i = 0;\n  do {\n    var names = Object.getOwnPropertyNames(obj);\n    for (i = 0; i < names.length; i++) {\n      props [names[i]] = null;\n    }\n  } while (obj = Object.getPrototypeOf(obj));\n  return props;\n};\n" :: JSString)
  _ <- eval ("/* virtual-dom diffing algorithm, applies patches as detected */\nwindow = typeof window === 'undefined' ? {} : window;\nwindow['diff'] = function diff(currentObj, newObj, parent, doc) {\n  if (!currentObj && !newObj) return;\n  else if (!currentObj && newObj) window['createNode'](newObj, parent, doc);\n  else if (currentObj && !newObj) window['destroyNode'](currentObj, parent);\n  else {\n    if (currentObj.type === 'vtext') {\n      if (newObj.type === 'vnode') window['replaceTextWithElement'](currentObj, newObj, parent, doc);\n      else window['diffTextNodes'](currentObj, newObj);\n    } else {\n      if (newObj.type === 'vnode') window['diffVNodes'](currentObj, newObj, parent, doc);\n      else window['replaceElementWithText'](currentObj, newObj, parent, doc);\n    }\n  }\n};\n\nwindow['destroyNode'] = function destroyNode(obj, parent) {\n  window['callBeforeDestroyedRecursive'](obj);\n  parent.removeChild(obj['domRef']);\n  window['callDestroyedRecursive'](obj);\n};\n\nwindow['callDestroyedRecursive'] = function callDestroyedRecursive(obj) {\n  window['callDestroyed'](obj);\n  for (var i in obj.children)\n    window['callDestroyedRecursive'](obj.children[i]);\n};\n\nwindow['callDestroyed'] = function callDestroyed(obj) {\n  if (obj['onDestroyed']) obj['onDestroyed']();\n};\n\nwindow['callBeforeDestroyed'] = function callBeforeDestroyed(obj) {\n  if (obj['onBeforeDestroyed']) obj['onBeforeDestroyed']();\n};\n\nwindow['callBeforeDestroyedRecursive'] = function callBeforeDestroyedRecursive(obj) {\n  window['callBeforeDestroyed'](obj);\n  for (var i in obj.children)\n    window['callBeforeDestroyedRecursive'](obj.children[i]);\n};\n\nwindow['diffTextNodes'] = function diffTextNodes(c, n) {\n  if (c['text'] !== n['text']) c['domRef'].textContent = n['text'];\n  n['domRef'] = c['domRef'];\n};\n\nwindow['replaceElementWithText'] = function replaceElementWithText(c, n, parent, doc) {\n  n['domRef'] = doc.createTextNode(n['text']);\n  window['callBeforeDestroyedRecursive'](c);\n  parent.replaceChild(n['domRef'], c['domRef']);\n  window['callDestroyedRecursive'](c);\n};\n\nwindow['replaceTextWithElement'] = function replaceTextWithElement(c, n, parent, doc) {\n  window['createElement'](n, doc);\n  parent.replaceChild(n['domRef'], c['domRef']);\n  window['callCreated'](n);\n};\n\nwindow['callCreated'] = function callCreated(obj) {\n  if (obj['onCreated']) obj['onCreated']();\n};\n\nwindow['populate'] = function populate(c, n, doc) {\n  if (!c) c = {\n    props: null,\n    css: null,\n    children: []\n  }\n  window['diffProps'](c['props'], n['props'], n['domRef'], n['ns'] === 'svg');\n  window['diffCss'](c['css'], n['css'], n['domRef']);\n  window['diffChildren'](c['children'], n['children'], n['domRef'], doc);\n};\n\nwindow['diffVNodes'] = function diffVNodes(c, n, parent, doc) {\n  if (c['tag'] === n['tag'] && n['key'] === c['key']) {\n    n['domRef'] = c['domRef'];\n    window['populate'](c, n, doc);\n  } else {\n    window['createElement'](n, doc);\n    window['callBeforeDestroyedRecursive'](c);\n    parent.replaceChild(n['domRef'], c['domRef']);\n    window['callDestroyedRecursive'](c);\n    window['callCreated'](n);\n  }\n};\n\nwindow['diffProps'] = function diffProps(cProps, nProps, node, isSvg) {\n  var newProp;\n  /* Is current prop in new prop list? */\n  for (var c in cProps) {\n    newProp = nProps[c];\n    /* If current property no longer exists, remove it */\n    if (newProp === undefined) {\n      /* current key is not in node, remove it from DOM, if SVG, remove attribute */\n      if (isSvg || !(c in node))\n        node.removeAttribute(c, cProps[c]);\n      else\n        node[c] = '';\n    } else {\n      /* Already on DOM from previous diff, continue */\n      if (newProp === cProps[c] && c !== 'checked' && c !== 'value') continue;\n      if (isSvg) {\n        if (c === 'href')\n          node.setAttributeNS('http://www.w3.org/1999/xlink', 'href', newProp);\n        else\n          node.setAttribute(c, newProp);\n      } else if (c in node && !(c === 'list' || c === 'form')) {\n        node[c] = newProp;\n      } else {\n        node.setAttribute(c, newProp);\n      }\n    }\n  }\n  /* add remaining */\n  for (var n in nProps) {\n    if (cProps && cProps[n]) continue;\n    newProp = nProps[n];\n    /* Only add new properties, skip (continue) if they already exist in current property map */\n    if (isSvg) {\n      if (n === 'href')\n        node.setAttributeNS('http://www.w3.org/1999/xlink', 'href', newProp);\n      else\n        node.setAttribute(n, newProp);\n    } else if (n in node && !(n === 'list' || n === 'form')) {\n      node[n] = nProps[n];\n    } else {\n      node.setAttribute(n, newProp);\n    }\n  }\n};\n\nwindow['diffCss'] = function diffCss(cCss, nCss, node) {\n  var result;\n  /* is current attribute in new attribute list? */\n  for (var c in cCss) {\n    result = nCss[c];\n    if (!result) {\n      /* current key is not in node */\n      node.style[c] = null;\n    } else if (result !== cCss[c]) {\n      node.style[c] = result;\n    }\n  }\n  /* add remaining */\n  for (var n in nCss) {\n    if (cCss && cCss[n]) continue;\n    node.style[n] = nCss[n];\n  }\n};\n\nwindow['hasKeys'] = function hasKeys(ns, cs) {\n  return ns.length > 0 && cs.length > 0 && ns[0]['key'] != null && cs[0]['key'] != null;\n};\n\nwindow['diffChildren'] = function diffChildren(cs, ns, parent, doc) {\n  var longest = ns.length > cs.length ? ns.length : cs.length;\n  if (window['hasKeys'](ns, cs)) {\n    window['syncChildren'](cs, ns, parent, doc);\n  } else {\n    for (var i = 0; i < longest; i++)\n      window['diff'](cs[i], ns[i], parent, doc);\n  }\n};\n\nwindow['createElement'] = function createElement(obj, doc) {\n  if (obj['ns'] === 'svg') {\n    obj['domRef'] = doc.createElementNS('http://www.w3.org/2000/svg', obj['tag']);\n  } else if (obj['ns'] === 'mathml') {\n    obj['domRef'] = doc.createElementNS('http://www.w3.org/1998/Math/MathML', obj['tag']);\n  } else {\n    obj['domRef'] = doc.createElement(obj['tag']);\n  }\n  window['populate'](null, obj, doc);\n};\n\nwindow['createNode'] = function createNode(obj, parent, doc) {\n  if (obj.type === 'vnode') window['createElement'](obj, doc);\n  else obj['domRef'] = doc.createTextNode(obj['text']);\n  parent.appendChild(obj['domRef']);\n  window['callCreated'](obj);\n};\n\n/* Child reconciliation algorithm, inspired by kivi and Bobril */\nwindow['syncChildren'] = function syncChildren(os, ns, parent, doc) {\n  var oldFirstIndex = 0,\n    newFirstIndex = 0,\n    oldLastIndex = os.length - 1,\n    newLastIndex = ns.length - 1,\n    nFirst, nLast, oLast, oFirst, tmp, found, node;\n  for (;;) {\n    /* check base case, first > last for both new and old\n      [ ] -- old children empty (fully-swapped)\n      [ ] -- new children empty (fully-swapped)\n      */\n    if (newFirstIndex > newLastIndex && oldFirstIndex > oldLastIndex) {\n      break;\n    }\n\n    /* Initialize */\n    nFirst = ns[newFirstIndex];\n    nLast = ns[newLastIndex];\n    oFirst = os[oldFirstIndex];\n    oLast = os[oldLastIndex];\n    /* No more old nodes, create and insert all remaining nodes\n       -> [ ] <- old children\n       -> [ a b c ] <- new children\n       */\n    if (oldFirstIndex > oldLastIndex) {\n      window['diff'](null, nFirst, parent, doc);\n      /* insertBefore's semantics will append a node if the second argument provided is `null` or `undefined`.\n         Otherwise, it will insert node['domRef'] before oLast['domRef']. */\n      parent.insertBefore(nFirst['domRef'], oFirst ? oFirst['domRef'] : null);\n      os.splice(newFirstIndex, 0, nFirst);\n      newFirstIndex++;\n    }\n    /* No more new nodes, delete all remaining nodes in old list\n       -> [ a b c ] <- old children\n       -> [ ] <- new children\n       */\n    else if (newFirstIndex > newLastIndex) {\n      tmp = oldLastIndex;\n      while (oldLastIndex >= oldFirstIndex) {\n        parent.removeChild(os[oldLastIndex--]['domRef']);\n      }\n      os.splice(oldFirstIndex, tmp - oldFirstIndex + 1);\n      break;\n    }\n    /* happy path, everything aligns, we continue\n       -> oldFirstIndex -> [ a b c ] <- oldLastIndex\n       -> newFirstIndex -> [ a b c ] <- newLastIndex\n       check if nFirst and oFirst align, if so, check nLast and oLast\n       */\n    else if (oFirst['key'] === nFirst['key']) {\n      window['diff'](os[oldFirstIndex++], ns[newFirstIndex++], parent, doc);\n    } else if (oLast['key'] === nLast['key']) {\n      window['diff'](os[oldLastIndex--], ns[newLastIndex--], parent, doc);\n    }\n    /* flip-flop case, nodes have been swapped, in some way or another\n       both could have been swapped.\n       -> [ a b c ] <- old children\n       -> [ c b a ] <- new children\n       */\n    else if (oFirst['key'] === nLast['key'] && nFirst['key'] === oLast['key']) {\n      window['swapDomRefs'](node, oLast['domRef'], oFirst['domRef'], parent);\n      window['swap'](os, oldFirstIndex, oldLastIndex);\n      window['diff'](os[oldFirstIndex++], ns[newFirstIndex++], parent, doc);\n      window['diff'](os[oldLastIndex--], ns[newLastIndex--], parent, doc);\n    }\n    /* Or just one could be swapped (d's align here)\n           This is top left and bottom right match case.\n           We move d to end of list, mutate old vdom to reflect the change\n           We then continue without affecting indexes, hoping to land in a better case\n           -> [ d a b ] <- old children\n           -> [ a b d ] <- new children\n           becomes\n           -> [ a b d ] <- old children\n           -> [ a b d ] <- new children\n           and now we happy path\n           */\n    else if (oFirst['key'] === nLast['key']) {\n      /* insertAfter */\n      parent.insertBefore(oFirst['domRef'], oLast['domRef'].nextSibling);\n      /* swap positions in old vdom */\n      os.splice(oldLastIndex,0,os.splice(oldFirstIndex,1)[0]);\n      window['diff'](os[oldLastIndex--], ns[newLastIndex--], parent, doc);\n    }\n    /* This is top right and bottom lefts match case.\n       We move d to end of list, mutate old vdom to reflect the change\n       -> [ b a d ] <- old children\n       -> [ d b a ] <- new children\n       becomes\n       -> [ d b a ] <- old children\n       -> [ d b a ] <- new children\n       and now we happy path\n       */\n    else if (oLast['key'] === nFirst['key']) {\n      /* insertAfter */\n      parent.insertBefore(oLast['domRef'], oFirst['domRef']);\n      /* swap positions in old vdom */\n      os.splice(oldFirstIndex,0, os.splice(oldLastIndex,1)[0]);\n      window['diff'](os[oldFirstIndex++], nFirst, parent, doc);\n      newFirstIndex++;\n    }\n\n    /* The 'you're screwed' case, nothing aligns, pull the ripcord, do something more fancy\n       This can happen when the list is sorted, for example.\n       -> [ a e c ] <- old children\n       -> [ b e d ] <- new children\n       */\n    else {\n      /* final case, perform linear search to check if new key exists in old map, decide what to do from there */\n      found = false;\n      tmp = oldFirstIndex;\n      while (tmp <= oldLastIndex) {\n        if (os[tmp]['key'] === nFirst['key']) {\n          found = true;\n          node = os[tmp];\n          break;\n        }\n        tmp++;\n      }\n      /* If new key was found in old map this means it was moved, hypothetically as below\n         -> [ a e b c ] <- old children\n         -> [ b e a j ] <- new children\n          ^\n         In the above case 'b' has been moved, so we need to insert 'b' before 'a' in both vDOM and DOM\n         We also increase oldFirstIndex and newFirstIndex.\n\n         This results in new list below w/ updated index position\n         -> [ b a e c ] <- old children\n         -> [ b e a j ] <- new children\n            ^\n            */\n      if (found) {\n        /* Move item to correct position */\n        os.splice(oldFirstIndex,0, os.splice(tmp,1)[0]);\n        /* optionally perform `diff` here */\n        window['diff'](os[oldFirstIndex++], nFirst, parent, doc);\n        /* Swap DOM references */\n        parent.insertBefore(node['domRef'], os[oldFirstIndex]['domRef']);\n        /* increment counters */\n        newFirstIndex++;\n      }\n      /* If new key was *not* found in the old map this means it must now be created, example below\n           -> [ a e d c ] <- old children\n           -> [ b e a j ] <- new children\n            ^\n\n           In the above case 'b' does not exist in the old map, so we create a new element and DOM reference.\n           We then insertBefore in both vDOM and DOM.\n\n           -> [ b a e d c ] <- old children\n           -> [ b e a j   ] <- new children\n              ^\n              */\n      else {\n        window['createElement'](nFirst, doc);\n        parent.insertBefore(nFirst['domRef'], oFirst['domRef']);\n        os.splice(oldFirstIndex++, 0, nFirst);\n        newFirstIndex++;\n        oldLastIndex++;\n      }\n    }\n  }\n};\n\nwindow['swapDomRefs'] = function swapDomRefs(tmp,a,b,p) {\n  tmp = a.nextSibling;\n  p.insertBefore(a,b);\n  p.insertBefore(b,tmp);\n};\n\nwindow['swap']= function swap(os,l,r) {\n  var k = os[l];\n  os[l] = os[r];\n  os[r] = k;\n};\n" :: JSString)
  _ <- eval ("window = typeof window === 'undefined' ? {} : window;\nwindow['collapseSiblingTextNodes'] = function collapseSiblingTextNodes(vs) {\n  if (!vs) { return []; }\n  var ax = 0, adjusted = vs.length > 0 ? [vs[0]] : [];\n  for (var ix = 1; ix < vs.length; ix++) {\n    if (adjusted[ax]['type'] === 'vtext' && vs[ix]['type'] === 'vtext') {\n      adjusted[ax]['text'] += vs[ix]['text'];\n      continue;\n    }\n    adjusted[++ax] = vs[ix];\n  }\n  return adjusted;\n}\n\nwindow['copyDOMIntoVTree'] = function copyDOMIntoVTree(logLevel,mountPoint, vtree, doc) {\n  if (!doc) { doc = window.document; }\n  var mountChildIdx = 0, node;\n  // If script tags are rendered first in body, skip them.\n  if (!mountPoint) {\n    if (doc.body.childNodes.length > 0) {\n      node = doc.body.firstChild;\n    } else {\n      node = doc.body.appendChild (doc.createElement('div'));\n    }\n  } else if (mountPoint.childNodes.length === 0) {\n    node = mountPoint.appendChild (doc.createElement('div'));\n  } else {\n    while (mountPoint.childNodes[mountChildIdx] && (mountPoint.childNodes[mountChildIdx].nodeType === Node.TEXT_NODE || mountPoint.childNodes[mountChildIdx].localName === 'script')){\n      mountChildIdx++;\n    }\n    if (!mountPoint.childNodes[mountChildIdx]) {\n      node = doc.body.appendChild (doc.createElement('div'));\n    } else {\n      node = mountPoint.childNodes[mountChildIdx];\n    }\n  }\n\n  if (!window['walk'](logLevel,vtree, node, doc)) {\n    if (logLevel) {\n      console.warn('Could not copy DOM into virtual DOM, falling back to diff');\n    }\n    // Remove all children before rebuilding DOM\n    while (node.firstChild) node.removeChild(node.lastChild);\n    vtree['domRef'] = node;\n    window['populate'](null, vtree, doc);\n    return false;\n  }\n  if (logLevel) {\n    console.info ('Successfully prendered page');\n  }\n  return true;\n}\n\nwindow['diagnoseError'] = function diagnoseError(logLevel, vtree, node) {\n  if (logLevel) console.warn('VTree differed from node', vtree, node);\n}\n\nwindow['walk'] = function walk(logLevel, vtree, node, doc) {\n  // This is slightly more complicated than one might expect since\n  // browsers will collapse consecutive text nodes into a single text node.\n  // There can thus be fewer DOM nodes than VDOM nodes.\n  var vdomChild,\n    domChild;\n\n  vtree['domRef'] = node;\n\n  // Fire onCreated events as though the elements had just been created.\n  window['callCreated'](vtree);\n\n  vtree.children = window['collapseSiblingTextNodes'](vtree.children);\n  for (var i = 0; i < vtree.children.length; i++) {\n    vdomChild = vtree['children'][i];\n    domChild = node.childNodes[i];\n    if (!domChild) {\n      window['diagnoseError'](logLevel,vdomChild, domChild);\n      return false;\n    }\n    if (vdomChild.type === 'vtext') {\n      if (domChild.nodeType !== Node.TEXT_NODE) {\n        window['diagnoseError'](logLevel, vdomChild, domChild);\n        return false;\n      }\n\n      if (vdomChild['text'] === domChild.textContent) {\n        vdomChild['domRef'] = domChild;\n      } else {\n        window['diagnoseError'](logLevel, vdomChild, domChild);\n        return false;\n      }\n    } else {\n      if (domChild.nodeType !== Node.ELEMENT_NODE) return false;\n      vdomChild['domRef'] = domChild;\n      if(!window['walk'](logLevel, vdomChild, domChild, doc)) return false;\n    }\n  }\n  return true;\n}\n" :: JSString)
  _ <- eval ("window = typeof window === 'undefined' ? {} : window;\nwindow['callFocus'] = function callFocus(id) {\n  setTimeout(function(){\n    var ele = document.getElementById(id);\n    if (ele && ele.focus) ele.focus()\n  }, 50);\n}\n\nwindow['callBlur'] = function callBlur(id) {\n  setTimeout(function(){\n    var ele = document.getElementById(id);\n    if (ele && ele.blur) ele.blur()\n  }, 50);\n}\n" :: JSString)
#endif
#endif
  -- init Notifier
  Notify {..} <- liftIO newNotify
  -- init empty actions
  actionsRef <- liftIO (newIORef S.empty)
  let writeEvent a = void . liftIO . forkIO $ do
        atomicModifyIORef' actionsRef $ \as -> (as |> a, ())
        notify
  -- init Subs
  forM_ subs $ \sub ->
    sub writeEvent
  -- Hack to get around `BlockedIndefinitelyOnMVar` exception
  -- that occurs when no event handlers are present on a template
  -- and `notify` is no longer in scope
  void . liftIO . forkIO . forever $ threadDelay (1000000 * 86400) >> notify
  -- Retrieves reference view
  viewRef <- getView writeEvent
  -- know thy mountElement
  mountEl <- mountElement mountPoint
  -- Begin listening for events in the virtual dom
  delegator mountEl viewRef events
  -- Process initial action of application
  writeEvent initialAction
  -- Program loop, blocking on SkipChan

  let
    loop !oldModel = liftIO wait >> do
        -- Apply actions to model
        actions <- liftIO $ atomicModifyIORef' actionsRef $ \actions -> (S.empty, actions)
        let (Acc newModel effects) = foldl' (foldEffects writeEvent update)
                                            (Acc oldModel (pure ())) actions
        effects
        oldName <- liftIO $ oldModel `seq` makeStableName oldModel
        newName <- liftIO $ newModel `seq` makeStableName newModel
        when (oldName /= newName && oldModel /= newModel) $ do
          swapCallbacks
          newVTree <- runView (view newModel) writeEvent
          oldVTree <- liftIO (readIORef viewRef)
          void $ waitForAnimationFrame
          (diff mountPoint) (Just oldVTree) (Just newVTree)
          releaseCallbacks
          liftIO (atomicWriteIORef viewRef newVTree)
        syncPoint
        loop newModel
  loop m

-- | Runs an isomorphic miso application.
-- Assumes the pre-rendered DOM is already present
miso :: Eq model => (URI -> App model action) -> JSM ()
miso f = do
  app@App {..} <- f <$> getCurrentURI
  common app model $ \writeEvent -> do
    let initialView = view model
    VTree (OI.Object iv) <- flip runView writeEvent initialView
    mountEl <- mountElement mountPoint
    -- Initial diff can be bypassed, just copy DOM into VTree
    copyDOMIntoVTree (logLevel == DebugPrerender) mountEl iv
    let initialVTree = VTree (OI.Object iv)
    -- Create virtual dom, perform initial diff
    liftIO (newIORef initialVTree)

-- | Runs a miso application
startApp :: Eq model => App model action -> JSM ()
startApp app@App {..} =
  common app model $ \writeEvent -> do
    let initialView = view model
    initialVTree <- flip runView writeEvent initialView
    (diff mountPoint) Nothing (Just initialVTree)
    liftIO (newIORef initialVTree)

-- | Helper
foldEffects
  :: Sink action
  -> (action -> model -> Effect action model)
  -> Acc model -> action -> Acc model
foldEffects sink update = \(Acc model as) action ->
  case update action model of
    Effect newModel effs -> Acc newModel newAs
      where
        newAs = as >> do
          forM_ effs $ \eff -> forkJSM (eff sink)

data Acc model = Acc !model !(JSM ())

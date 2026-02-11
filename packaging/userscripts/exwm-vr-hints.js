// ==UserScript==
// @name        EXWM-VR Link Hints
// @namespace   exwm-vr
// @description Extract clickable element positions for gaze-based link following.
//              Outputs JSON to QUTE_FIFO for consumption by ewwm-qutebrowser-gaze.el.
// @version     1.0.0
// ==/UserScript==

(function () {
    'use strict';

    var QUTE_FIFO = (typeof process !== 'undefined' && process.env && process.env.QUTE_FIFO)
        ? process.env.QUTE_FIFO
        : null;

    var SELECTORS = [
        'a[href]',
        'button',
        'input[type="submit"]',
        'input[type="button"]',
        'input[type="reset"]',
        '[onclick]',
        '[role="button"]',
        '[role="link"]',
        '[tabindex]',
        'summary',
        'label[for]'
    ].join(', ');

    function isVisible(el) {
        var rect = el.getBoundingClientRect();
        if (rect.width === 0 || rect.height === 0) return false;
        var style = window.getComputedStyle(el);
        if (style.display === 'none' || style.visibility === 'hidden') return false;
        if (parseFloat(style.opacity) < 0.1) return false;
        return true;
    }

    function getTextContent(el) {
        var text = (el.textContent || el.getAttribute('aria-label') || el.getAttribute('title') || '').trim();
        if (text.length > 80) text = text.substring(0, 77) + '...';
        return text;
    }

    function getHref(el) {
        if (el.tagName === 'A' && el.href) return el.href;
        var closest = el.closest('a[href]');
        if (closest) return closest.href;
        return null;
    }

    function collectHints() {
        var elements = document.querySelectorAll(SELECTORS);
        var hints = [];
        var id = 0;

        for (var i = 0; i < elements.length; i++) {
            var el = elements[i];
            if (!isVisible(el)) continue;

            var rect = el.getBoundingClientRect();
            hints.push({
                id: id,
                text: getTextContent(el),
                rect: {
                    x: Math.round(rect.left + window.scrollX),
                    y: Math.round(rect.top + window.scrollY),
                    w: Math.round(rect.width),
                    h: Math.round(rect.height)
                },
                url: getHref(el),
                tag: el.tagName.toLowerCase()
            });

            // Mark element for highlight targeting
            el.setAttribute('data-ewwm-hint', String(id));
            id++;
        }

        return hints;
    }

    var hints = collectHints();
    var output = JSON.stringify(hints);

    if (QUTE_FIFO) {
        // Node.js environment (qutebrowser userscript)
        var fs = require('fs');
        fs.writeFileSync(QUTE_FIFO, 'message-info ' + JSON.stringify('ewwm-hints:' + output) + '\n');
    } else {
        // Fallback: log to console for debugging
        console.log('ewwm-hints:' + output);
    }
})();

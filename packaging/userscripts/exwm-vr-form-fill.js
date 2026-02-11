// ==UserScript==
// @name        EXWM-VR Form Fill
// @namespace   exwm-vr
// @description Extract form field positions for KeePassXC auto-type integration.
//              Outputs JSON to QUTE_FIFO with field metadata and bounding rects.
// @version     1.0.0
// ==/UserScript==

(function () {
    'use strict';

    var QUTE_FIFO = (typeof process !== 'undefined' && process.env && process.env.QUTE_FIFO)
        ? process.env.QUTE_FIFO
        : null;

    var INPUT_SELECTORS = [
        'input[type="text"]',
        'input[type="password"]',
        'input[type="email"]',
        'input[type="tel"]',
        'input[type="url"]',
        'input[type="search"]',
        'input[type="number"]',
        'input:not([type])',
        'textarea',
        'select'
    ].join(', ');

    function isVisible(el) {
        var rect = el.getBoundingClientRect();
        if (rect.width === 0 || rect.height === 0) return false;
        var style = window.getComputedStyle(el);
        return style.display !== 'none' && style.visibility !== 'hidden';
    }

    function getLabel(el) {
        // Check for associated label
        if (el.id) {
            var label = document.querySelector('label[for="' + el.id + '"]');
            if (label) return label.textContent.trim();
        }
        // Check parent label
        var parentLabel = el.closest('label');
        if (parentLabel) return parentLabel.textContent.trim();
        // Fall back to placeholder or name
        return el.placeholder || el.name || el.getAttribute('aria-label') || '';
    }

    var elements = document.querySelectorAll(INPUT_SELECTORS);
    var fields = [];
    var id = 0;

    for (var i = 0; i < elements.length; i++) {
        var el = elements[i];
        if (!isVisible(el)) continue;

        var rect = el.getBoundingClientRect();
        fields.push({
            id: id,
            name: el.name || '',
            type: el.type || el.tagName.toLowerCase(),
            label: getLabel(el),
            autocomplete: el.getAttribute('autocomplete') || '',
            rect: {
                x: Math.round(rect.left + window.scrollX),
                y: Math.round(rect.top + window.scrollY),
                w: Math.round(rect.width),
                h: Math.round(rect.height)
            }
        });
        id++;
    }

    var output = JSON.stringify(fields);

    if (QUTE_FIFO) {
        var fs = require('fs');
        fs.writeFileSync(QUTE_FIFO, 'message-info ' + JSON.stringify('ewwm-fields:' + output) + '\n');
    } else {
        console.log('ewwm-fields:' + output);
    }
})();

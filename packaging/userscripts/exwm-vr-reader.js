// ==UserScript==
// @name        EXWM-VR Reader Mode
// @namespace   exwm-vr
// @description Extract main article content and inject VR-optimized reading CSS.
//              Uses a simple readability heuristic (largest text block).
// @version     1.0.0
// ==/UserScript==

(function () {
    'use strict';

    // CSS parameters from EWWM environment variables
    var FONT_SIZE = (typeof process !== 'undefined' && process.env && process.env.EWWM_READER_FONT_SIZE)
        ? process.env.EWWM_READER_FONT_SIZE : '22';
    var LINE_HEIGHT = (typeof process !== 'undefined' && process.env && process.env.EWWM_READER_LINE_HEIGHT)
        ? process.env.EWWM_READER_LINE_HEIGHT : '1.6';
    var MAX_WIDTH = (typeof process !== 'undefined' && process.env && process.env.EWWM_READER_MAX_WIDTH)
        ? process.env.EWWM_READER_MAX_WIDTH : '700';
    var FONT_FAMILY = (typeof process !== 'undefined' && process.env && process.env.EWWM_READER_FONT_FAMILY)
        ? process.env.EWWM_READER_FONT_FAMILY : 'serif';
    var DARK_MODE = (typeof process !== 'undefined' && process.env && process.env.EWWM_READER_DARK_MODE)
        ? process.env.EWWM_READER_DARK_MODE !== '0' : true;

    function findMainContent() {
        // Try semantic elements first
        var candidates = ['article', 'main', '[role="main"]', '.post-content',
                          '.entry-content', '.article-body', '#content'];
        for (var i = 0; i < candidates.length; i++) {
            var el = document.querySelector(candidates[i]);
            if (el && el.textContent.trim().length > 200) return el;
        }

        // Fallback: find the element with the most text
        var paragraphs = document.querySelectorAll('p');
        var bestParent = null;
        var bestScore = 0;

        for (var j = 0; j < paragraphs.length; j++) {
            var parent = paragraphs[j].parentElement;
            if (!parent) continue;
            var score = parent.textContent.trim().length;
            if (score > bestScore) {
                bestScore = score;
                bestParent = parent;
            }
        }
        return bestParent || document.body;
    }

    function countWords(text) {
        return text.split(/\s+/).filter(function (w) { return w.length > 0; }).length;
    }

    var content = findMainContent();
    var title = document.title || '';
    var wordCount = countWords(content.textContent);

    var bg = DARK_MODE ? '#1a1a2e' : '#fafafa';
    var fg = DARK_MODE ? '#e0e0e0' : '#2d2d2d';

    var css = 'body { background: ' + bg + '; color: ' + fg +
        '; font-size: ' + FONT_SIZE + 'px; line-height: ' + LINE_HEIGHT +
        '; max-width: ' + MAX_WIDTH + 'px; margin: 0 auto; padding: 2em;' +
        ' font-family: ' + FONT_FAMILY + '; }' +
        ' img { max-width: 100%; height: auto; }' +
        ' pre, code { overflow-x: auto; }';

    document.head.innerHTML = '<style>' + css + '</style><title>' + title + '</title>';
    document.body.innerHTML = '<h1>' + title + '</h1>' +
        '<p style="opacity:0.6">' + wordCount + ' words</p>' +
        content.innerHTML;
})();

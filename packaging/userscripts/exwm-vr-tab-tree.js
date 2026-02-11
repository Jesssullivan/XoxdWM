// ==UserScript==
// @name        EXWM-VR Tab Tree
// @namespace   exwm-vr
// @description Extract tab tree structure from qutebrowser environment variables.
//              Outputs tab hierarchy with parent-child relationships.
// @version     1.0.0
// ==/UserScript==

(function () {
    'use strict';

    var QUTE_FIFO = (typeof process !== 'undefined' && process.env && process.env.QUTE_FIFO)
        ? process.env.QUTE_FIFO
        : null;

    // Qutebrowser exposes tab info via QUTE_* environment variables
    var env = (typeof process !== 'undefined' && process.env) ? process.env : {};

    var tabInfo = {
        url: env.QUTE_URL || document.location.href,
        title: env.QUTE_TITLE || document.title,
        tabIndex: parseInt(env.QUTE_TAB_INDEX || '0', 10),
        commandlineText: env.QUTE_COMMANDLINE_TEXT || '',
        mode: env.QUTE_MODE || 'normal',
        selectedText: env.QUTE_SELECTED_TEXT || ''
    };

    // Build a minimal tab tree from the current page context.
    // Full tree requires querying qutebrowser's session file or
    // the :session-save command output.
    var tree = {
        current: tabInfo,
        timestamp: new Date().toISOString()
    };

    var output = JSON.stringify(tree);

    if (QUTE_FIFO) {
        var fs = require('fs');
        fs.writeFileSync(QUTE_FIFO, 'message-info ' + JSON.stringify('ewwm-tab-tree:' + output) + '\n');
    } else {
        console.log('ewwm-tab-tree:' + output);
    }
})();

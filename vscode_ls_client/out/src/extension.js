/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const net = require("net");
const vscode_1 = require("vscode");
const vscode_languageclient_1 = require("vscode-languageclient");
console.log("hello 2");
let client;
var Configuration;
(function (Configuration) {
    let configurationListener;
    // Convert VS Code specific settings to a format acceptable by the server. Since
    // both client and server do use JSON the conversion is trivial. 
    function computeConfiguration(params, _token, _next) {
        if (!params.items) {
            return null;
        }
        let result = [];
        for (let item of params.items) {
            // The server asks the client for configuration settings without a section
            // If a section is present we return null to indicate that the configuration
            // is not supported.
            if (item.section) {
                result.push(null);
                continue;
            }
            let config;
            if (item.scopeUri) {
                config = vscode_1.workspace.getConfiguration('lspMultiRootSample', client.protocol2CodeConverter.asUri(item.scopeUri));
            }
            else {
                config = vscode_1.workspace.getConfiguration('lspMultiRootSample');
            }
            result.push({
                maxNumberOfProblems: config.get('maxNumberOfProblems')
            });
        }
        return result;
    }
    Configuration.computeConfiguration = computeConfiguration;
    function initialize() {
        // VS Code currently doesn't sent fine grained configuration changes. So we 
        // listen to any change. However this will change in the near future.
        configurationListener = vscode_1.workspace.onDidChangeConfiguration(() => {
            console.log("config changed");
            client.sendNotification(vscode_languageclient_1.DidChangeConfigurationNotification.type, { settings: null });
        });
    }
    Configuration.initialize = initialize;
    function dispose() {
        if (configurationListener) {
            configurationListener.dispose();
        }
    }
    Configuration.dispose = dispose;
})(Configuration || (Configuration = {}));
function activate(_context) {
    //let socketTransport = { kind: TransportKind.socket, port: 8080 }
    const port = 9999;
    console.log("Start stuff");
    let serverOptions = function () {
        return new Promise((resolve, _reject) => {
            var client = new net.Socket();
            client.connect(port, "127.0.0.1", function () {
                resolve({
                    reader: client,
                    writer: client
                });
            });
            client.on("data", (data) => console.log("data", data.toString()));
            client.on("error", (data) => console.log("error", data.toString()));
        });
    };
    let middleware = {
        workspace: {
            configuration: Configuration.computeConfiguration
        }
    };
    // Options to control the language client
    let clientOptions = {
        // Register the server for plain text documents
        documentSelector: [
            { scheme: 'file', language: 'erlang' },
            { scheme: 'file', language: 'plaintext' },
            { pattern: '**/*.erl' }
        ],
        synchronize: {
            // Notify the server about file changes to '.clientrc files contain in the workspace
            fileEvents: [
                vscode_1.workspace.createFileSystemWatcher('**/.clientrc'),
                vscode_1.workspace.createFileSystemWatcher('**/*.erl'),
            ],
        },
        middleware: middleware
    };
    // Create the language client and start the client.
    client = new vscode_languageclient_1.LanguageClient('languageServerExample', 'Language Server Example', serverOptions, clientOptions);
    // Register new proposed protocol if available.
    client.registerProposedFeatures();
    client.onReady().then(() => {
        Configuration.initialize();
        client.trace = (vscode_languageclient_1.Trace.fromString('verbose'));
        console.log("LS initialised");
    });
    client.trace = (vscode_languageclient_1.Trace.fromString('verbose'));
    // Start the client. This will also launch the server
    client.start();
}
exports.activate = activate;
function deactivate() {
    console.log("Language server deactivated");
    if (!client) {
        return undefined;
    }
    Configuration.dispose();
    return client.stop();
}
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map
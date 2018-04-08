/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
'use strict';

import * as net from 'net';
import { workspace, ExtensionContext, WorkspaceConfiguration, Disposable } from 'vscode';
import { 
	LanguageClient, LanguageClientOptions, ServerOptions, CancellationToken, Middleware, 
	DidChangeConfigurationNotification, Proposed, ProposedFeatures, Trace
} from 'vscode-languageclient';

console.log("hello 2");

// The example settings
interface MultiRootExampleSettings {
	maxNumberOfProblems: number;
}

let client: LanguageClient;

namespace Configuration {

	let configurationListener: Disposable;

	// Convert VS Code specific settings to a format acceptable by the server. Since
	// both client and server do use JSON the conversion is trivial. 
	export function computeConfiguration(params: Proposed.ConfigurationParams, _token: CancellationToken, _next: Function): any[] {
		if (!params.items) {
			return null;
		}
		let result: (MultiRootExampleSettings | null)[] = [];
		for (let item of params.items) {
			// The server asks the client for configuration settings without a section
			// If a section is present we return null to indicate that the configuration
			// is not supported.
			if (item.section) {
				result.push(null);
				continue;
			}
			let config: WorkspaceConfiguration;
			if (item.scopeUri) {
				config = workspace.getConfiguration('lspMultiRootSample', client.protocol2CodeConverter.asUri(item.scopeUri));
			} else {
				config = workspace.getConfiguration('lspMultiRootSample');
			}
			result.push({
				maxNumberOfProblems: config.get('maxNumberOfProblems')
			});
		}
		return result;
	}
	
	export function initialize() {
		// VS Code currently doesn't sent fine grained configuration changes. So we 
		// listen to any change. However this will change in the near future.
		configurationListener = workspace.onDidChangeConfiguration(() => {
			console.log("config changed");
			client.sendNotification(DidChangeConfigurationNotification.type, { settings: null });
		});
	}

	export function dispose() {
		if (configurationListener) {
			configurationListener.dispose();
		}
	}
}


export function activate(_context: ExtensionContext) {
	//let socketTransport = { kind: TransportKind.socket, port: 8080 }
	const port = 9999;
	console.log("Start stuff");
	let serverOptions: ServerOptions = function() {
		return new Promise((resolve, _reject) => {
			var client = new net.Socket();
			client.connect(port, "127.0.0.1", function() {
				resolve({
					reader: client,
					writer: client
				});
			});
			client.on("data", (data) => console.log("data",data.toString() ));
			client.on("error", (data) => console.log("error",data.toString()));
		});
	}

	let middleware: ProposedFeatures.ConfigurationMiddleware | Middleware = {
		workspace: {
			configuration: Configuration.computeConfiguration
		}
	};

	// Options to control the language client
	let clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [
			{scheme: 'file', language: 'erlang'},
			{scheme: 'file', language: 'plaintext'},
			{pattern: '**/*.erl'}
		],
		synchronize: {
			// Notify the server about file changes to '.clientrc files contain in the workspace
			fileEvents: [
				workspace.createFileSystemWatcher('**/.clientrc'),
				workspace.createFileSystemWatcher('**/*.erl'),
			],
			// In the past this told the client to actively synchronize settings. Since the
			// client now supports 'getConfiguration' requests this active synchronization is not
			// necessary anymore. 
			// configurationSection: [ 'lspMultiRootSample' ]
		},
		middleware: middleware as Middleware
	}
	
	// Create the language client and start the client.
	client = new LanguageClient('languageServerExample', 'Language Server Example', serverOptions, clientOptions);
	// Register new proposed protocol if available.
	client.registerProposedFeatures();

	client.onReady().then(() => {
		Configuration.initialize();
		client.trace = (Trace.fromString('verbose'));
		console.log("LS initialised");
	});
	client.trace = (Trace.fromString('verbose'));

	// Start the client. This will also launch the server
	client.start();
}

export function deactivate(): Thenable<void> {
	console.log("Language server deactivated");
	if (!client) {
		return undefined;
	}
	Configuration.dispose();
	return client.stop();
}
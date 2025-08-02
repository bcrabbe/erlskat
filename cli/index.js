#!/usr/bin/env node

const importJsx = require('import-jsx');
const React = require('react');
const { render } = require('ink');
const yargs = require('yargs/yargs');
const { hideBin } = require('yargs/helpers');
const App = importJsx('./src/app');

// Parse command line arguments
const argv = yargs(hideBin(process.argv))
  .option('host', {
    alias: 'h',
    type: 'string',
    description: 'Erlskat server host',
    default: process.env.ERLSKAT_HOST || 'localhost'
  })
  .option('port', {
    alias: 'p',
    type: 'number',
    description: 'Erlskat server port',
    default: parseInt(process.env.ERLSKAT_PORT) || 8080
  })
  .option('debug', {
    alias: 'd',
    type: 'boolean',
    description: 'Enable debug logging',
    default: false
  })
  .help()
  .argv;

// Validate configuration
if (!argv.host || !argv.port) {
  console.error('Error: Host and port must be specified');
  process.exit(1);
}

if (argv.port < 1 || argv.port > 65535) {
  console.error('Error: Port must be between 1 and 65535');
  process.exit(1);
}

// Create WebSocket URL
const wsUrl = `ws://${argv.host}:${argv.port}/ws`;

// Log connection info if debug enabled
if (argv.debug) {
  console.log(`Connecting to: ${wsUrl}`);
}

// Render the app
const { unmount } = render(React.createElement(App, {
  wsUrl,
  debug: argv.debug
}));

// Handle graceful shutdown
process.on('SIGINT', () => {
  unmount();
  process.exit(0);
});

process.on('SIGTERM', () => {
  unmount();
  process.exit(0);
});

#!/usr/bin/env node

/**
 * Simple connection test for erlskat CLI
 * Tests WebSocket connection and basic message handling without UI
 */

const WebSocketManager = require('./src/websocket');
const MessageHandler = require('./src/utils/messageHandler');
const SessionManager = require('./src/session');

async function testConnection() {
  console.log('🃏 Testing Erlskat CLI Connection...\n');

  const wsUrl = 'ws://localhost:8080/ws';
  const wsManager = new WebSocketManager(wsUrl, true);
  const messageHandler = new MessageHandler(true);
  const sessionManager = new SessionManager(true);

  console.log('📡 Testing session management...');
  await sessionManager.loadSession();
  console.log('✅ Session management OK\n');

  console.log('🔌 Testing WebSocket connection...');
  
  let connected = false;
  let receivedMessages = 0;

  wsManager.setConnectionChangeHandler((state) => {
    console.log(`📶 Connection state: ${state}`);
    if (state === 'connected') {
      connected = true;
    }
  });

  wsManager.setMessageHandler((message) => {
    receivedMessages++;
    console.log(`📨 Received message #${receivedMessages}: ${message.type || 'unknown'}`);
    messageHandler.handleMessage(message);
  });

  messageHandler.setStateUpdateHandler((gameState) => {
    console.log(`🎮 Game state update: Phase=${gameState.phase}, Hand=${gameState.playerHand?.length || 0} cards`);
  });

  messageHandler.setBroadcastHandler((message) => {
    console.log(`📢 Broadcast: ${message.message || message.type}`);
  });

  wsManager.setErrorHandler((error) => {
    console.error(`❌ WebSocket error: ${error.message}`);
  });

  // Connect and wait a few seconds
  await wsManager.connect();
  
  await new Promise(resolve => setTimeout(resolve, 3000));

  console.log('\n📊 Test Results:');
  console.log(`✅ Connection established: ${connected}`);
  console.log(`✅ Messages received: ${receivedMessages}`);
  console.log(`✅ Game state: ${messageHandler.getGameState().phase}`);

  if (connected && receivedMessages > 0) {
    console.log('\n🎉 All tests passed! CLI is ready to use.');
  } else {
    console.log('\n⚠️  Some tests failed. Check server connection.');
  }

  wsManager.disconnect();
  console.log('\n👋 Test complete.');
}

// Run test
testConnection().catch(console.error);
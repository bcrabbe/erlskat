/**
 * Message handler for processing erlskat websocket messages
 * Maps message types to their handlers and manages game state transitions
 */

/**
 * Categorize message types for handling
 */
const MESSAGE_CATEGORIES = {
  // Messages that require user input
  PROMPT: [
    'bid_prompt',
    'game_type_prompt', 
    'multiplier_prompt',
    'initial_choice_prompt',
    'discard_prompt',
    'card_play_prompt'
  ],
  
  // Messages that update game state but don't require input
  BROADCAST: [
    'awaiting_bid',
    'bidding_winner_notification',
    'game_declaration_broadcast',
    'game_type_broadcast',
    'hand_reorder_broadcast',
    'bid_broadcast',
    'pass_broadcast',
    'game_start_broadcast',
    'card_played_broadcast',
    'trick_won_broadcast',
    'game_complete_broadcast',
    'scores_update_broadcast',
    'next_hand_starting_broadcast'
  ],
  
  // Messages that update player state
  STATE_UPDATE: [
    'skat_flipped',
    'hand_with_skat',
    'hand_after_discard',
    'bidding_complete',
    'bidding_roles',
    'cards_dealt'
  ],
  
  // Connection and lobby management
  CONNECTION: [
    'player_disconnected',
    'player_timed_out', 
    'game_closed',
    'lobby_status',
    'player_joined',
    'table_started'
  ],
  
  // Error messages
  ERROR: [
    'invalid_card_error',
    'card_play_error',
    'error'
  ]
};

class MessageHandler {
  constructor(debug = false) {
    this.debug = debug;
    this.gameState = {
      phase: 'lobby', // lobby, bidding, playing, complete, disconnected
      playerHand: null,
      currentPrompt: null,
      gameInfo: {},
      connectionInfo: {}
    };
    
    // Event handlers
    this.onStateUpdate = null;
    this.onPrompt = null;
    this.onBroadcast = null;
    this.onError = null;
    this.onConnectionChange = null;
  }

  /**
   * Process incoming message and route to appropriate handler
   */
  handleMessage(message) {
    if (!message || typeof message !== 'object') {
      this.handleError('Invalid message format');
      return;
    }

    const messageType = message.type;
    if (!messageType) {
      this.handleError('Message missing type field');
      return;
    }

    if (this.debug) {
      console.log(`Processing message type: ${messageType}`);
    }

    try {
      // Route message to appropriate category handler
      if (MESSAGE_CATEGORIES.PROMPT.includes(messageType)) {
        this.handlePromptMessage(message);
      } else if (MESSAGE_CATEGORIES.BROADCAST.includes(messageType)) {
        this.handleBroadcastMessage(message);
      } else if (MESSAGE_CATEGORIES.STATE_UPDATE.includes(messageType)) {
        this.handleStateUpdateMessage(message);
      } else if (MESSAGE_CATEGORIES.CONNECTION.includes(messageType)) {
        this.handleConnectionMessage(message);
      } else if (MESSAGE_CATEGORIES.ERROR.includes(messageType)) {
        this.handleErrorMessage(message);
      } else {
        this.handleError(`Unknown message type: ${messageType}`);
      }
    } catch (error) {
      this.handleError(`Error processing message: ${error.message}`);
    }
  }

  /**
   * Handle messages that require user input
   */
  handlePromptMessage(message) {
    this.gameState.currentPrompt = message;
    
    // Update phase based on prompt type
    switch (message.type) {
      case 'bid_prompt':
      case 'initial_choice_prompt':
      case 'game_type_prompt':
      case 'multiplier_prompt':
      case 'discard_prompt':
        this.gameState.phase = 'bidding';
        break;
      case 'card_play_prompt':
        this.gameState.phase = 'playing';
        // Update hand from prompt if provided
        if (message.hand) {
          this.gameState.playerHand = message.hand;
        }
        break;
    }
    
    this.notifyStateUpdate();
    
    if (this.onPrompt) {
      this.onPrompt(message);
    }
  }

  /**
   * Handle broadcast messages (informational)
   */
  handleBroadcastMessage(message) {
    // Store relevant game information
    switch (message.type) {
      case 'game_start_broadcast':
        this.gameState.phase = 'playing';
        this.gameState.gameInfo = {
          declarer: message.declarer,
          gameType: message.game_type,
          isHandGame: message.is_hand_game,
          selectedMultipliers: message.selected_multipliers
        };
        break;
      case 'game_complete_broadcast':
        this.gameState.phase = 'complete';
        this.gameState.gameInfo.result = message.result;
        break;
    }
    
    this.notifyStateUpdate();
    
    if (this.onBroadcast) {
      this.onBroadcast(message);
    }
  }

  /**
   * Handle state update messages
   */
  handleStateUpdateMessage(message) {
    switch (message.type) {
      case 'cards_dealt':
        this.gameState.playerHand = message.hand;
        this.gameState.phase = 'bidding';
        break;
      case 'hand_with_skat':
        this.gameState.playerHand = message.cards;
        break;
      case 'hand_after_discard':
        this.gameState.playerHand = message.hand;
        break;
      case 'hand_reorder_broadcast':
        // Update hand if this message contains our reordered hand
        if (message.hands && message.hands.length > 0) {
          this.gameState.playerHand = message.hands[0].hand;
        }
        break;
      case 'skat_flipped':
        this.gameState.gameInfo.skat = message.cards;
        break;
      case 'bidding_complete':
        this.gameState.gameInfo.biddingResult = message.result;
        break;
    }
    
    this.notifyStateUpdate();
  }

  /**
   * Handle connection and lobby messages
   */
  handleConnectionMessage(message) {
    switch (message.type) {
      case 'lobby_status':
        this.gameState.phase = 'lobby';
        this.gameState.connectionInfo.lobbyState = message.state;
        this.gameState.connectionInfo.players = message.players;
        break;
      case 'table_started':
        this.gameState.phase = 'bidding';
        this.gameState.connectionInfo.players = message.players;
        break;
      case 'game_closed':
        this.resetGameState();
        break;
      case 'player_disconnected':
        this.gameState.connectionInfo.disconnectedPlayer = message.player_id;
        this.gameState.connectionInfo.reconnectDeadline = message.reconnection_deadline_ms;
        break;
      case 'player_timed_out':
        this.gameState.connectionInfo.timedOutPlayer = message.player_id;
        break;
    }
    
    this.notifyStateUpdate();
    
    if (this.onConnectionChange) {
      this.onConnectionChange(message);
    }
  }

  /**
   * Handle error messages
   */
  handleErrorMessage(message) {
    if (this.onError) {
      this.onError(message);
    }
  }

  /**
   * Handle general errors
   */
  handleError(errorMessage) {
    if (this.debug) {
      console.error('MessageHandler error:', errorMessage);
    }
    
    if (this.onError) {
      this.onError({ type: 'error', message: errorMessage });
    }
  }

  /**
   * Reset game state (on game_closed)
   */
  resetGameState() {
    this.gameState = {
      phase: 'lobby',
      playerHand: null,
      currentPrompt: null,
      gameInfo: {},
      connectionInfo: {}
    };
    this.notifyStateUpdate();
  }

  /**
   * Get current game state
   */
  getGameState() {
    return { ...this.gameState };
  }

  /**
   * Notify state update
   */
  notifyStateUpdate() {
    if (this.onStateUpdate) {
      this.onStateUpdate(this.getGameState());
    }
  }

  /**
   * Set event handlers
   */
  setStateUpdateHandler(handler) {
    this.onStateUpdate = handler;
  }

  setPromptHandler(handler) {
    this.onPrompt = handler;
  }

  setBroadcastHandler(handler) {
    this.onBroadcast = handler;
  }

  setErrorHandler(handler) {
    this.onError = handler;
  }

  setConnectionChangeHandler(handler) {
    this.onConnectionChange = handler;
  }
}

module.exports = MessageHandler;
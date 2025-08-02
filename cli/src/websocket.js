const WebSocket = require('ws');
const SessionManager = require('./session');

/**
 * WebSocket connection handler for erlskat CLI
 * Manages connection lifecycle, session cookies, and message handling
 */
class WebSocketManager {
  constructor(wsUrl, debug = false) {
    this.wsUrl = wsUrl;
    this.debug = debug;
    this.ws = null;
    this.sessionManager = new SessionManager(debug);
    this.connectionState = 'disconnected'; // disconnected, connecting, connected
    this.reconnectAttempts = 0;
    this.maxReconnectAttempts = 5;
    this.reconnectDelay = 1000; // Start with 1 second
    
    // Event handlers
    this.onMessage = null;
    this.onConnectionChange = null;
    this.onError = null;
  }

  /**
   * Initialize connection with session management
   */
  async connect() {
    try {
      this.connectionState = 'connecting';
      this.notifyConnectionChange();

      // Load existing session if available
      await this.sessionManager.loadSession();
      
      // Prepare connection options
      const options = {
        headers: {}
      };

      // Add session cookie if available
      const cookieHeader = this.sessionManager.formatCookieHeader();
      if (cookieHeader) {
        options.headers.Cookie = cookieHeader;
        if (this.debug) {
          console.log('Connecting with session cookie');
        }
      }

      // Create WebSocket connection
      this.ws = new WebSocket(this.wsUrl, options);
      
      this.ws.on('open', this.handleOpen.bind(this));
      this.ws.on('message', this.handleMessage.bind(this));
      this.ws.on('close', this.handleClose.bind(this));
      this.ws.on('error', this.handleError.bind(this));

    } catch (error) {
      this.handleError(error);
    }
  }

  /**
   * Handle WebSocket open event
   */
  async handleOpen() {
    this.connectionState = 'connected';
    this.reconnectAttempts = 0;
    this.reconnectDelay = 1000;
    
    if (this.debug) {
      console.log('WebSocket connected to', this.wsUrl);
    }
    
    this.notifyConnectionChange();

    // Check for session cookie in response headers
    // Note: In WebSocket, we typically get session cookies during the handshake
    // The session will be handled when we receive the first message from server
  }

  /**
   * Handle incoming WebSocket messages
   */
  handleMessage(data) {
    try {
      const message = JSON.parse(data.toString());
      
      if (this.debug) {
        console.log('Received message:', message);
      }

      // Handle session establishment (this might come in the first response)
      // In practice, the session cookie would be set during the HTTP upgrade
      // but we'll check for any session-related messages
      
      if (this.onMessage) {
        this.onMessage(message);
      }
    } catch (error) {
      if (this.debug) {
        console.error('Error parsing message:', error.message);
      }
      
      if (this.onError) {
        this.onError(new Error(`Failed to parse message: ${error.message}`));
      }
    }
  }

  /**
   * Handle WebSocket close event
   */
  handleClose(code, reason) {
    this.connectionState = 'disconnected';
    
    if (this.debug) {
      console.log(`WebSocket closed: ${code} - ${reason}`);
    }
    
    this.notifyConnectionChange();
    
    // Attempt reconnection if not intentionally closed
    if (code !== 1000 && this.reconnectAttempts < this.maxReconnectAttempts) {
      this.scheduleReconnection();
    }
  }

  /**
   * Handle WebSocket errors
   */
  handleError(error) {
    if (this.debug) {
      console.error('WebSocket error:', error.message);
    }
    
    if (this.onError) {
      this.onError(error);
    }
  }

  /**
   * Schedule reconnection with exponential backoff
   */
  scheduleReconnection() {
    this.reconnectAttempts++;
    
    if (this.debug) {
      console.log(`Scheduling reconnection attempt ${this.reconnectAttempts}/${this.maxReconnectAttempts} in ${this.reconnectDelay}ms`);
    }
    
    setTimeout(() => {
      if (this.connectionState === 'disconnected') {
        this.connect();
      }
    }, this.reconnectDelay);
    
    // Exponential backoff with jitter
    this.reconnectDelay = Math.min(this.reconnectDelay * 2 + Math.random() * 1000, 30000);
  }

  /**
   * Send message to server
   */
  sendMessage(message) {
    if (this.connectionState !== 'connected' || !this.ws) {
      throw new Error('WebSocket not connected');
    }
    
    try {
      const messageStr = JSON.stringify(message);
      this.ws.send(messageStr);
      
      if (this.debug) {
        console.log('Sent message:', message);
      }
    } catch (error) {
      if (this.debug) {
        console.error('Error sending message:', error.message);
      }
      throw error;
    }
  }

  /**
   * Gracefully close connection
   */
  disconnect() {
    if (this.ws) {
      this.ws.close(1000, 'Client disconnect');
    }
  }

  /**
   * Get current connection state
   */
  getConnectionState() {
    return this.connectionState;
  }

  /**
   * Clear session (for permanent disconnection)
   */
  async clearSession() {
    await this.sessionManager.clearSession();
  }

  /**
   * Notify connection state change
   */
  notifyConnectionChange() {
    if (this.onConnectionChange) {
      this.onConnectionChange(this.connectionState);
    }
  }

  /**
   * Set event handlers
   */
  setMessageHandler(handler) {
    this.onMessage = handler;
  }

  setConnectionChangeHandler(handler) {
    this.onConnectionChange = handler;
  }

  setErrorHandler(handler) {
    this.onError = handler;
  }
}

module.exports = WebSocketManager;
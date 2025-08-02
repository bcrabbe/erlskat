const fs = require('fs-extra');
const path = require('path');
const os = require('os');

const SESSION_FILE = path.join(os.homedir(), '.erlskat-session');

/**
 * Session management for erlskat CLI
 * Handles persistence of skat_session cookie between reconnections
 */
class SessionManager {
  constructor(debug = false) {
    this.debug = debug;
    this.session = null;
  }

  /**
   * Load existing session from filesystem
   * @returns {string|null} Session cookie value or null if not found
   */
  async loadSession() {
    try {
      if (await fs.pathExists(SESSION_FILE)) {
        const sessionData = await fs.readJson(SESSION_FILE);
        
        // Validate session structure
        if (sessionData && sessionData.cookie && sessionData.timestamp) {
          // Check if session is not too old (24 hours)
          const sessionAge = Date.now() - sessionData.timestamp;
          const maxAge = 24 * 60 * 60 * 1000; // 24 hours
          
          if (sessionAge < maxAge) {
            this.session = sessionData.cookie;
            if (this.debug) {
              console.log('Loaded existing session from', SESSION_FILE);
            }
            return this.session;
          } else {
            if (this.debug) {
              console.log('Session expired, removing old session file');
            }
            await this.clearSession();
          }
        }
      }
    } catch (error) {
      if (this.debug) {
        console.error('Error loading session:', error.message);
      }
      // If there's an error reading, clear the session file
      await this.clearSession();
    }
    
    return null;
  }

  /**
   * Save session to filesystem
   * @param {string} cookie - Session cookie value
   */
  async saveSession(cookie) {
    try {
      const sessionData = {
        cookie: cookie,
        timestamp: Date.now()
      };
      
      await fs.writeJson(SESSION_FILE, sessionData, { spaces: 2 });
      this.session = cookie;
      
      if (this.debug) {
        console.log('Session saved to', SESSION_FILE);
      }
    } catch (error) {
      if (this.debug) {
        console.error('Error saving session:', error.message);
      }
    }
  }

  /**
   * Clear session from filesystem and memory
   */
  async clearSession() {
    try {
      if (await fs.pathExists(SESSION_FILE)) {
        await fs.remove(SESSION_FILE);
        if (this.debug) {
          console.log('Session file removed');
        }
      }
      this.session = null;
    } catch (error) {
      if (this.debug) {
        console.error('Error clearing session:', error.message);
      }
    }
  }

  /**
   * Get current session cookie
   * @returns {string|null} Current session cookie or null
   */
  getSession() {
    return this.session;
  }

  /**
   * Parse Set-Cookie header and extract skat_session value
   * @param {string[]} setCookieHeaders - Array of Set-Cookie header values
   * @returns {string|null} Extracted session value or null
   */
  parseSessionFromHeaders(setCookieHeaders) {
    if (!setCookieHeaders || !Array.isArray(setCookieHeaders)) {
      return null;
    }

    for (const cookieHeader of setCookieHeaders) {
      // Parse cookie header format: "skat_session=value; Path=/; HttpOnly"
      const match = cookieHeader.match(/skat_session=([^;]+)/);
      if (match) {
        return match[1];
      }
    }

    return null;
  }

  /**
   * Format cookie for WebSocket headers
   * @returns {string|null} Formatted cookie string or null
   */
  formatCookieHeader() {
    if (!this.session) {
      return null;
    }
    return `skat_session=${this.session}`;
  }
}

module.exports = SessionManager;
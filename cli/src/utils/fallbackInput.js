const readline = require('readline');

/**
 * Fallback input handler for when raw mode is not supported
 * Uses readline for basic input functionality
 */
class FallbackInputHandler {
  constructor() {
    this.rl = null;
    this.currentPrompt = null;
    this.onInput = null;
  }

  /**
   * Initialize the fallback input handler
   * @param {Function} inputHandler - Function to call with input
   */
  initialize(inputHandler) {
    this.onInput = inputHandler;
    
    // Create readline interface for non-raw mode input
    this.rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout,
      prompt: '> '
    });

    // Handle input lines
    this.rl.on('line', (input) => {
      this.handleInput(input.trim());
    });

    this.rl.on('close', () => {
      console.log('\nGoodbye!');
      process.exit(0);
    });
  }

  /**
   * Handle user input and convert to appropriate game commands
   * @param {string} input - User input string
   */
  handleInput(input) {
    if (!this.onInput) return;

    const lowerInput = input.toLowerCase();

    // Handle bidding commands
    if (lowerInput === 'h' || lowerInput === 'hold') {
      this.onInput('hold');
    } else if (lowerInput === 'p' || lowerInput === 'pass') {
      this.onInput('pass');
    }
    // Handle hand/skat choice
    else if (lowerInput === 's' || lowerInput === 'skat') {
      this.onInput('skat');
    } else if (lowerInput === 'hand') {
      this.onInput('hand');
    }
    // Handle card selection by index
    else if (/^\d+$/.test(input)) {
      const cardIndex = parseInt(input);
      this.onInput('card_select', cardIndex);
    }
    // Handle multiple card selection (e.g., "0,1" or "0 1")
    else if (/^[\d\s,]+$/.test(input)) {
      const indices = input.split(/[,\s]+/).map(s => parseInt(s.trim())).filter(n => !isNaN(n));
      if (indices.length > 0) {
        this.onInput('multi_card_select', indices);
      }
    }
    // Handle game type selection
    else if (input.includes('diamonds') || lowerInput === 'd') {
      this.onInput('diamonds');
    } else if (input.includes('hearts') || lowerInput === 'h') {
      this.onInput('hearts');
    } else if (input.includes('spades') || lowerInput === 's') {
      this.onInput('spades');
    } else if (input.includes('clubs') || lowerInput === 'c') {
      this.onInput('clubs');
    } else if (input.includes('grand') || lowerInput === 'g') {
      this.onInput('grand');
    } else if (input.includes('null') || lowerInput === 'n') {
      this.onInput('null');
    }
    // Handle help command
    else if (lowerInput === 'help' || lowerInput === '?') {
      this.showHelp();
    } else {
      console.log('Unknown command. Type "help" for available commands.');
    }

    // Re-prompt for next input
    this.prompt();
  }

  /**
   * Set the current prompt message
   * @param {string} promptText - Text to show as prompt
   */
  setPrompt(promptText) {
    this.currentPrompt = promptText;
    if (this.rl) {
      console.log(`\n${promptText}`);
      this.prompt();
    }
  }

  /**
   * Show the input prompt
   */
  prompt() {
    if (this.rl) {
      this.rl.prompt();
    }
  }

  /**
   * Show help information
   */
  showHelp() {
    console.log(`
Available Commands:
  Bidding: h/hold, p/pass
  Hand Choice: h/hand, s/skat
  Game Types: d/diamonds, h/hearts, s/spades, c/clubs, g/grand, n/null
  Card Selection: [number] (e.g., 0, 1, 2...)
  Multi-Card: [numbers] (e.g., "0,1" or "0 1")
  Help: help, ?
  Exit: Ctrl+C
`);
  }

  /**
   * Clean up readline interface
   */
  close() {
    if (this.rl) {
      this.rl.close();
    }
  }
}

module.exports = FallbackInputHandler;
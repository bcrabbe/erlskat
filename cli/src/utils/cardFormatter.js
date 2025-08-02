/**
 * Card formatting utilities for displaying Skat cards in the CLI
 */

// Suit symbols mapping
const SUIT_SYMBOLS = {
  'clubs': '♣',
  'spades': '♠', 
  'hearts': '♥',
  'diamonds': '♦'
};

// Rank display mapping
const RANK_DISPLAY = {
  'seven': '7',
  'eight': '8', 
  'nine': '9',
  'ten': '10',
  'jack': 'J',
  'queen': 'Q',
  'king': 'K',
  'ace': 'A'
};

/**
 * Format a single card for display
 * @param {Object} card - Card object with rank and suit properties
 * @returns {string} Formatted card string
 */
function formatCard(card) {
  if (!card || !card.rank || !card.suit) {
    return '[??]';
  }
  
  const rank = RANK_DISPLAY[card.rank] || card.rank;
  const suit = SUIT_SYMBOLS[card.suit] || card.suit;
  
  return `${rank}${suit}`;
}

/**
 * Format a card with padding for consistent width
 * @param {Object} card - Card object
 * @param {number} width - Desired width (default 4)
 * @returns {string} Padded card string
 */
function formatCardPadded(card, width = 4) {
  const cardStr = formatCard(card);
  return cardStr.padEnd(width);
}

/**
 * Create a visual card box for display
 * @param {Object} card - Card object
 * @param {boolean} selected - Whether card is selected
 * @param {boolean} elevated - Whether card should be elevated (selected in hand)
 * @returns {string[]} Array of strings representing card lines
 */
function createCardBox(card, selected = false, elevated = false) {
  const cardStr = formatCard(card);
  const width = Math.max(6, cardStr.length + 2);
  
  // Create card border
  const topBorder = '┌' + '─'.repeat(width - 2) + '┐';
  const bottomBorder = '└' + '─'.repeat(width - 2) + '┘';
  const cardLine = '│' + cardStr.padEnd(width - 2) + '│';
  
  // Visual indicators
  const selectionIndicator = selected ? '▲' : ' ';
  const selectionLine = selectionIndicator.padStart(Math.floor(width / 2) + 1);
  
  const cardLines = [topBorder, cardLine, bottomBorder];
  
  if (selected && !elevated) {
    cardLines.push(selectionLine);
  }
  
  return cardLines;
}

/**
 * Format a hand of cards for horizontal display
 * @param {Array} hand - Array of card objects
 * @param {number} selectedIndex - Index of selected card (-1 for none)
 * @param {Array} multiSelected - Array of selected indices for multi-selection
 * @returns {string[]} Array of lines to display the hand
 */
function formatHand(hand, selectedIndex = -1, multiSelected = []) {
  if (!hand || hand.length === 0) {
    return ['No cards in hand'];
  }
  
  const cardBoxes = hand.map((card, index) => {
    const isSelected = selectedIndex === index;
    const isMultiSelected = multiSelected.includes(index);
    return createCardBox(card, isSelected || isMultiSelected, isSelected);
  });
  
  // Determine max height (elevated cards are taller)
  const maxHeight = Math.max(...cardBoxes.map(box => box.length));
  
  // Create lines by combining card boxes horizontally
  const lines = [];
  for (let lineIndex = 0; lineIndex < maxHeight; lineIndex++) {
    const line = cardBoxes.map((box, cardIndex) => {
      // Handle elevated cards (selected card starts higher)
      const isElevated = selectedIndex === cardIndex;
      const startOffset = isElevated ? 0 : 1;
      const adjustedIndex = lineIndex - startOffset;
      
      if (adjustedIndex >= 0 && adjustedIndex < box.length) {
        return box[adjustedIndex];
      } else {
        // Calculate box width for padding
        const boxWidth = box[0] ? box[0].length : 6;
        return ' '.repeat(boxWidth);
      }
    }).join(' ');
    
    lines.push(line);
  }
  
  return lines;
}

/**
 * Format cards with indices for selection
 * @param {Array} hand - Array of card objects
 * @returns {string} Formatted string with indexed cards
 */
function formatHandWithIndices(hand) {
  if (!hand || hand.length === 0) {
    return 'No cards in hand';
  }
  
  return hand.map((card, index) => {
    return `${index}: ${formatCard(card)}`;
  }).join('  ');
}

/**
 * Format a trick (cards played in current round)
 * @param {Array} trick - Array of played card objects with player info
 * @returns {string[]} Array of lines displaying the trick
 */
function formatTrick(trick) {
  if (!trick || trick.length === 0) {
    return ['No cards played'];
  }
  
  const lines = ['Current trick:'];
  trick.forEach(play => {
    const cardStr = formatCard(play.card);
    const playerStr = play.player_id || 'Unknown';
    lines.push(`  ${playerStr}: ${cardStr}`);
  });
  
  return lines;
}

/**
 * Create a visual representation of card selection state
 * @param {number} selectedIndex - Currently selected card index
 * @param {Array} multiSelected - Array of selected indices
 * @param {number} handSize - Total number of cards
 * @returns {string} Selection indicator line
 */
function createSelectionIndicator(selectedIndex, multiSelected, handSize) {
  if (handSize === 0) return '';
  
  const indicators = [];
  for (let i = 0; i < handSize; i++) {
    if (i === selectedIndex) {
      indicators.push('↑');
    } else if (multiSelected.includes(i)) {
      indicators.push('*');
    } else {
      indicators.push(' ');
    }
  }
  
  return indicators.join('    ');
}

module.exports = {
  formatCard,
  formatCardPadded,
  createCardBox,
  formatHand,
  formatHandWithIndices,
  formatTrick,
  createSelectionIndicator,
  SUIT_SYMBOLS,
  RANK_DISPLAY
};
const React = require('react');
const { Box, Text } = require('ink');
const { formatCard } = require('../utils/cardFormatter');

/**
 * Hand display component - shows player's cards at bottom of screen
 * Handles visual selection and multi-selection states
 */
const HandDisplay = ({
  hand = [],
  selectedIndex = -1,
  multiSelected = [],
  title = "Your Hand",
  showSelection = false
}) => {
  if (!hand || hand.length === 0) {
    return null; // Don't render if no cards
  }

  // Format cards as [rank+suit] with spacing using shared formatter
  const formatCardWithBrackets = (card) => {
    return `[${formatCard(card)}]`;
  };

  // Create the card row with proper spacing
  const cardRow = hand.map((card, index) => {
    const cardStr = formatCardWithBrackets(card);
    const isMultiSelected = multiSelected.includes(index);
    // Add spacing between cards (2 spaces)
    return (index === 0 ? '' : '  ') + cardStr;
  }).join('');

  // Create selection indicator row if needed
  let selectionRow = '';
  if (showSelection && selectedIndex >= 0 && selectedIndex < hand.length) {
    let position = 0;
    for (let i = 0; i < selectedIndex; i++) {
      const cardStr = formatCardWithBrackets(hand[i]);
      position += cardStr.length + (i > 0 ? 2 : 0); // Add spacing
    }
    // Center the arrow under the selected card
    const cardStr = formatCardWithBrackets(hand[selectedIndex]);
    const centerOffset = Math.floor(cardStr.length / 2);
    selectionRow = ' '.repeat(position + centerOffset) + '↑';
  }

  return React.createElement(Box, { 
    flexDirection: "column", 
    borderStyle: "single", 
    borderColor: "blue", 
    padding: 1 
  },
    React.createElement(Text, { bold: true, color: "blue" }, `─ ${title} ${'─'.repeat(Math.max(0, 44 - title.length))}`),
    React.createElement(Text, null, cardRow),
    showSelection && selectionRow && React.createElement(Text, { color: "yellow" }, selectionRow),
    showSelection && selectedIndex >= 0 && React.createElement(Text, { color: "yellow" }, "   (selected)"),
    multiSelected.length > 0 && React.createElement(Text, { color: "green" }, 
      `Selected: ${multiSelected.join(', ')}${multiSelected.length === 2 ? " (Press Shift+Enter to submit)" : ""}`
    )
  );
};

module.exports = HandDisplay;
const importJsx = require('import-jsx');
const React = require('react');
const { useState, useEffect } = require('react');
const { Box, Text } = require('ink');
const useSafeInput = require('../utils/safeInput');
const HandDisplay = importJsx('./HandDisplay');
const { formatTrick } = require('../utils/cardFormatter');

/**
 * Main game screen component with card selection logic
 */
const GameScreen = ({
  gameState,
  onCardSelection,
  onChoiceSelection,
  debug = false
}) => {
  const [selectedIndex, setSelectedIndex] = useState(0);
  const [multiSelected, setMultiSelected] = useState([]);
  const [messages, setMessages] = useState([]);

  const { phase, playerHand, currentPrompt, gameInfo } = gameState;

  // Reset selection when hand changes or prompt changes
  useEffect(() => {
    if (playerHand && playerHand.length > 0) {
      setSelectedIndex(Math.min(selectedIndex, playerHand.length - 1));
    }
    if (currentPrompt?.type === 'discard_prompt') {
      setMultiSelected([]);
    }
  }, [playerHand, currentPrompt]);

  // Add new messages to display
  useEffect(() => {
    if (currentPrompt?.message) {
      setMessages(prev => [...prev.slice(-10), currentPrompt.message]); // Keep last 10 messages
    }
  }, [currentPrompt]);

  // Handle keyboard input
  useSafeInput((input, key) => {
    if (!playerHand || playerHand.length === 0) return;

    // Arrow key navigation
    if (key.leftArrow) {
      setSelectedIndex(prev => (prev > 0 ? prev - 1 : playerHand.length - 1));
    } else if (key.rightArrow) {
      setSelectedIndex(prev => (prev < playerHand.length - 1 ? prev + 1 : 0));
    }
    // Handle card selection based on prompt type
    else if (key.return) {
      if (key.shift) {
        // Shift+Enter: Submit multi-selection
        if (currentPrompt?.type === 'discard_prompt' && multiSelected.length > 0) {
          onCardSelection(multiSelected);
          setMultiSelected([]);
        }
      } else {
        // Regular Enter: Different behavior based on prompt type
        if (currentPrompt?.type === 'card_play_prompt') {
          // Single card selection
          onCardSelection([selectedIndex]);
        } else if (currentPrompt?.type === 'discard_prompt') {
          // Multi-card selection toggle
          handleMultiCardToggle();
        }
      }
    }
  });

  const handleMultiCardToggle = () => {
    const maxCards = currentPrompt?.count || 2;
    
    if (multiSelected.includes(selectedIndex)) {
      // Deselect if already selected
      setMultiSelected(prev => prev.filter(i => i !== selectedIndex));
    } else {
      // Select card
      if (multiSelected.length < maxCards) {
        setMultiSelected(prev => [...prev, selectedIndex]);
      } else {
        // Replace first selected with new selection
        setMultiSelected(prev => [...prev.slice(1), selectedIndex]);
      }
    }
  };

  const renderPrompt = () => {
    if (!currentPrompt) return null;

    // Only render card-related prompts here
    // Non-card prompts are handled by PromptHandler component
    switch (currentPrompt.type) {
      case 'discard_prompt':
        return (
          <Box flexDirection="column">
            <Text color="yellow">{currentPrompt.message}</Text>
            <Text>Use arrow keys to navigate, Enter to select/deselect cards</Text>
            <Text>Press Shift+Enter when {currentPrompt.count || 2} cards are selected</Text>
          </Box>
        );

      case 'card_play_prompt':
        return (
          <Box flexDirection="column">
            <Text color="yellow">{currentPrompt.message}</Text>
            <Text>Use arrow keys to select card, Enter to play</Text>
            {currentPrompt.current_trick && currentPrompt.current_trick.length > 0 && (
              <Box flexDirection="column">
                <Text color="cyan">Current trick:</Text>
                {formatTrick(currentPrompt.current_trick).map((line, i) => (
                  <Text key={i}>{line}</Text>
                ))}
              </Box>
            )}
          </Box>
        );

      default:
        // For non-card prompts, show nothing here as PromptHandler will handle them
        return null;
    }
  };

  return (
    <Box flexDirection="column" height="100%">
      {/* Message area */}
      <Box flexDirection="column" flexGrow={1} borderStyle="single" padding={1}>
        <Text bold color="green">Erlskat Game - Phase: {phase}</Text>
        
        {/* Game info */}
        {gameInfo.declarer && (
          <Text>
            Declarer: {gameInfo.declarer} | Game: {gameInfo.gameType}
            {gameInfo.isHandGame && ' (Hand)'} 
            {gameInfo.selectedMultipliers?.length > 0 && ` + ${gameInfo.selectedMultipliers.join(', ')}`}
          </Text>
        )}

        {/* Recent messages */}
        {messages.slice(-5).map((message, index) => (
          <Text key={index} dimColor>
            {message}
          </Text>
        ))}

        {/* Current prompt */}
        {renderPrompt()}
      </Box>

      {/* Hand display */}
      {playerHand && playerHand.length > 0 && (
        <HandDisplay
          hand={playerHand}
          selectedIndex={selectedIndex}
          multiSelected={multiSelected}
        />
      )}

      {/* Debug info */}
      {debug && (
        <Box borderStyle="single" borderColor="gray">
          <Text color="gray">
            Debug: Selected={selectedIndex}, Multi={JSON.stringify(multiSelected)}, 
            Prompt={currentPrompt?.type || 'none'}
          </Text>
        </Box>
      )}
    </Box>
  );
};

module.exports = GameScreen;
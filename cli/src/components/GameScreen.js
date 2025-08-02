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
    // Handle non-card prompts first
    if (currentPrompt && !['card_play_prompt', 'discard_prompt'].includes(currentPrompt.type)) {
      switch (currentPrompt.type) {
        case 'bid_prompt':
          if (input === 'h' || input === 'H') {
            onChoiceSelection('hold');
          } else if (input === 'p' || input === 'P') {
            onChoiceSelection('pass');
          }
          break;

        case 'initial_choice_prompt':
          if (input === 'h' || input === 'H') {
            onChoiceSelection('hand');
          } else if (input === 's' || input === 'S') {
            onChoiceSelection('skat');
          }
          break;

        case 'game_type_prompt':
          const gameTypeNum = parseInt(input);
          if (gameTypeNum >= 1 && gameTypeNum <= (currentPrompt.game_types?.length || 0)) {
            const selectedGameType = currentPrompt.game_types[gameTypeNum - 1];
            onChoiceSelection(selectedGameType);
          }
          break;

        case 'multiplier_prompt':
          const multiplierNum = parseInt(input);
          if (multiplierNum >= 1 && multiplierNum <= (currentPrompt.multipliers?.length || 0)) {
            const selectedMultiplier = currentPrompt.multipliers[multiplierNum - 1];
            onChoiceSelection(selectedMultiplier);
          }
          break;
      }
      return;
    }

    // Handle card-related prompts
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

    switch (currentPrompt.type) {
      case 'bid_prompt':
        return (
          <Box flexDirection="column">
            <Text color="yellow">{currentPrompt.message}</Text>
            <Text>Use ↑/↓ arrows to navigate, Enter to select | Or press 'H' to Hold, 'P' to Pass</Text>
          </Box>
        );

      case 'initial_choice_prompt':
        return (
          <Box flexDirection="column">
            <Text color="yellow">{currentPrompt.message}</Text>
            <Text>Use ↑/↓ arrows to navigate, Enter to select | Or press 'H' for Hand, 'S' for Skat</Text>
          </Box>
        );

      case 'game_type_prompt':
        return (
          <Box flexDirection="column">
            <Text color="yellow">{currentPrompt.message}</Text>
            <Text>Choose a game type:</Text>
            {currentPrompt.game_types?.map((gameType, index) => {
              const valueInfo = currentPrompt.game_type_values?.find(v => v.game_type === gameType);
              return (
                <Text key={index} color="cyan">
                  {index + 1}: {gameType}{valueInfo ? ` (${valueInfo.value_display})` : ''}
                </Text>
              );
            })}
            <Text>Use ↑/↓ arrows to navigate, Enter to select | Or press number 1-{currentPrompt.game_types?.length || 0} to select</Text>
          </Box>
        );

      case 'multiplier_prompt':
        return (
          <Box flexDirection="column">
            <Text color="yellow">{currentPrompt.message}</Text>
            <Text>Game type: {currentPrompt.game_type}</Text>
            {currentPrompt.current_value && (
              <Text>Current value: {currentPrompt.current_value}</Text>
            )}
            <Text>Choose multiplier:</Text>
            {currentPrompt.multipliers?.map((multiplier, index) => {
              const valueInfo = currentPrompt.multiplier_values?.find(v => v.multiplier === multiplier);
              return (
                <Text key={index} color="cyan">
                  {index + 1}: {multiplier}{valueInfo ? ` (${valueInfo.value_display})` : ''}
                </Text>
              );
            })}
            <Text>Use ↑/↓ arrows to navigate, Enter to select | Or press number 1-{currentPrompt.multipliers?.length || 0} to select</Text>
          </Box>
        );

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

      case 'skat_flipped':
        return (
          <Box flexDirection="column">
            <Text color="yellow">{currentPrompt.message}</Text>
            <Text>Skat cards:</Text>
            {currentPrompt.cards?.map((card, index) => (
              <Text key={index} color="cyan">
                {card.rank} of {card.suit}
              </Text>
            ))}
          </Box>
        );

      default:
        return (
          <Box flexDirection="column">
            <Text color="yellow">{currentPrompt.message || 'Unknown prompt'}</Text>
            <Text color="gray">Prompt type: {currentPrompt.type}</Text>
            {debug && (
              <Text color="gray">
                Raw prompt: {JSON.stringify(currentPrompt, null, 2)}
              </Text>
            )}
          </Box>
        );
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

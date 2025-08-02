const React = require('react');
const { useState } = require('react');
const { Box, Text } = require('ink');
const useSafeInput = require('../utils/safeInput');

/**
 * Handles non-card prompts (bid, game type, multiplier, etc.)
 */
const PromptHandler = ({ 
  prompt, 
  onResponse,
  debug = false 
}) => {
  const [selectedOption, setSelectedOption] = useState(0);

  if (!prompt) return null;

  // Handle keyboard input for non-card prompts
  useSafeInput((input, key) => {
    // Handle arrow keys and enter for all prompt types with choices
    if (key.upArrow || key.downArrow || key.return) {
      const choices = getChoicesForPrompt(prompt);
      if (choices.length > 0) {
        if (key.upArrow) {
          setSelectedOption(prev => (prev - 1 + choices.length) % choices.length);
          return;
        } else if (key.downArrow) {
          setSelectedOption(prev => (prev + 1) % choices.length);
          return;
        } else if (key.return) {
          const selectedChoice = choices[selectedOption];
          onResponse(selectedChoice.value);
          return;
        }
      }
    }

    switch (prompt.type) {
      case 'bid_prompt':
        if (input === 'h' || input === 'H') {
          onResponse('hold');
        } else if (input === 'p' || input === 'P') {
          onResponse('pass');
        }
        break;

      case 'initial_choice_prompt':
        if (input === 'h' || input === 'H') {
          onResponse('hand');
        } else if (input === 's' || input === 'S') {
          onResponse('skat');
        }
        break;

      case 'game_type_prompt':
        // Handle number input for game type selection
        const gameTypeNum = parseInt(input);
        if (gameTypeNum >= 1 && gameTypeNum <= (prompt.game_types?.length || 0)) {
          const selectedGameType = prompt.game_types[gameTypeNum - 1];
          onResponse(selectedGameType);
        }
        break;

      case 'multiplier_prompt':
        // Handle number input for multiplier selection
        const multiplierNum = parseInt(input);
        if (multiplierNum >= 1 && multiplierNum <= (prompt.multipliers?.length || 0)) {
          const selectedMultiplier = prompt.multipliers[multiplierNum - 1];
          onResponse(selectedMultiplier);
        }
        break;

      default:
        if (debug) {
          console.log(`Unhandled prompt type: ${prompt.type}, input: ${input}`);
        }
        break;
    }
  });

  // Helper function to get choices for different prompt types
  const getChoicesForPrompt = (prompt) => {
    switch (prompt.type) {
      case 'bid_prompt':
        return [
          { value: 'hold', label: 'Hold' },
          { value: 'pass', label: 'Pass' }
        ];
      case 'initial_choice_prompt':
        return [
          { value: 'hand', label: 'Hand' },
          { value: 'skat', label: 'Skat' }
        ];
      case 'game_type_prompt':
        return prompt.game_types?.map(gameType => {
          const valueInfo = prompt.game_type_values?.find(v => v.game_type === gameType);
          return {
            value: gameType,
            label: `${gameType}${valueInfo ? ` (${valueInfo.value_display})` : ''}`
          };
        }) || [];
      case 'multiplier_prompt':
        return prompt.multipliers?.map(multiplier => {
          const valueInfo = prompt.multiplier_values?.find(v => v.multiplier === multiplier);
          return {
            value: multiplier,
            label: `${multiplier}${valueInfo ? ` (${valueInfo.value_display})` : ''}`
          };
        }) || [];
      default:
        return [];
    }
  };

  const renderPromptContent = () => {
    switch (prompt.type) {
      case 'bid_prompt':
        const bidChoices = getChoicesForPrompt(prompt);
        return (
          <Box flexDirection="column">
            <Text color="yellow" bold>{prompt.message}</Text>
            <Text>Bid value: {prompt.bid_value}</Text>
            {bidChoices.map((choice, index) => (
              <Text key={index} color={selectedOption === index ? "black" : "cyan"} backgroundColor={selectedOption === index ? "cyan" : undefined}>
                {choice.label}
              </Text>
            ))}
            <Text color="green">Use ↑/↓ arrows to navigate, Enter to select | Or press 'H' to Hold, 'P' to Pass</Text>
          </Box>
        );

      case 'initial_choice_prompt':
        const initialChoices = getChoicesForPrompt(prompt);
        return (
          <Box flexDirection="column">
            <Text color="yellow" bold>{prompt.message}</Text>
            {initialChoices.map((choice, index) => (
              <Text key={index} color={selectedOption === index ? "black" : "cyan"} backgroundColor={selectedOption === index ? "cyan" : undefined}>
                {choice.label}
              </Text>
            ))}
            <Text color="green">Use ↑/↓ arrows to navigate, Enter to select | Or press 'H' for Hand, 'S' for Skat</Text>
          </Box>
        );

      case 'game_type_prompt':
        const gameTypeChoices = getChoicesForPrompt(prompt);
        return (
          <Box flexDirection="column">
            <Text color="yellow" bold>{prompt.message}</Text>
            <Text>Choose a game type:</Text>
            {gameTypeChoices.map((choice, index) => (
              <Text key={index} color={selectedOption === index ? "black" : "cyan"} backgroundColor={selectedOption === index ? "cyan" : undefined}>
                {index + 1}: {choice.label}
              </Text>
            ))}
            <Text color="green">Use ↑/↓ arrows to navigate, Enter to select | Or press number 1-{gameTypeChoices.length} to select</Text>
          </Box>
        );

      case 'multiplier_prompt':
        const multiplierChoices = getChoicesForPrompt(prompt);
        return (
          <Box flexDirection="column">
            <Text color="yellow" bold>{prompt.message}</Text>
            <Text>Game type: {prompt.game_type}</Text>
            {prompt.current_value && (
              <Text>Current value: {prompt.current_value}</Text>
            )}
            <Text>Choose multiplier:</Text>
            {multiplierChoices.map((choice, index) => (
              <Text key={index} color={selectedOption === index ? "black" : "cyan"} backgroundColor={selectedOption === index ? "cyan" : undefined}>
                {index + 1}: {choice.label}
              </Text>
            ))}
            <Text color="green">Use ↑/↓ arrows to navigate, Enter to select | Or press number 1-{multiplierChoices.length} to select</Text>
          </Box>
        );

      case 'skat_flipped':
        return (
          <Box flexDirection="column">
            <Text color="yellow" bold>{prompt.message}</Text>
            <Text>Skat cards:</Text>
            {prompt.cards?.map((card, index) => (
              <Text key={index} color="cyan">
                {card.rank} of {card.suit}
              </Text>
            ))}
          </Box>
        );

      default:
        return (
          <Box flexDirection="column">
            <Text color="yellow" bold>{prompt.message || 'Unknown prompt'}</Text>
            <Text color="gray">Prompt type: {prompt.type}</Text>
            {debug && (
              <Text color="gray">
                Raw prompt: {JSON.stringify(prompt, null, 2)}
              </Text>
            )}
          </Box>
        );
    }
  };

  return (
    <Box flexDirection="column" borderStyle="single" borderColor="yellow" padding={1}>
      {renderPromptContent()}
    </Box>
  );
};

module.exports = PromptHandler;
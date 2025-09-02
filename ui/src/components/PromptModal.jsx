import React, { useState, useEffect } from 'react';
import './PromptModal.css';

const PromptModal = ({
  isOpen,
  message,
  choices = [],
  onChoice,
  onClose,
  type = 'choice',
  gameTypeValues = []
}) => {
  const [selectedIndex, setSelectedIndex] = useState(0);

  if (!isOpen) return null;

  const handleChoice = (choice) => {
    onChoice(choice);
    onClose();
  };

  const handleClose = () => {
    if (onClose) {
      onClose();
    }
  };

  useEffect(() => {
    const handleKeyDown = (e) => {
      if (e.key === 'ArrowLeft' || e.key === 'ArrowUp') {
        setSelectedIndex(prev => prev === 0 ? choices.length - 1 : prev - 1);
      } else if (e.key === 'ArrowRight' || e.key === 'ArrowDown') {
        setSelectedIndex(prev => (prev + 1) % choices.length);
      } else if (e.key === 'Enter') {
        handleChoice(choices[selectedIndex]);
      }
    };

    if (isOpen) {
      window.addEventListener('keydown', handleKeyDown);
    }

    return () => {
      window.removeEventListener('keydown', handleKeyDown);
    };
  }, [isOpen, selectedIndex, choices, type]);

  // Get the value display for game_type_prompt
  const getValueDisplay = () => {
    if (type === 'game_type_prompt' && gameTypeValues.length > 0) {
      const selectedChoice = choices[selectedIndex];
      const gameTypeValue = gameTypeValues.find(gtv => gtv.game_type === selectedChoice);
      return gameTypeValue ? gameTypeValue.value_display : null;
    }
    return null;
  };

  return (
    <div className="bid-prompt-overlay">
      <div className="bid-prompt-content">
        <div className="turn-indicator">{type === 'bid_prompt' ? "It's your turn to bid" : "Make your choice"}</div>
        <div className="bid-question">{message}</div>
        <div className="bid-choices">
          {choices.map((choice, index) => (
            <span
              key={index}
              className={`bid-option ${selectedIndex === index ? 'selected' : ''}`}
              onClick={() => {
                setSelectedIndex(index);
                handleChoice(choice);
              }}
              onMouseEnter={() => setSelectedIndex(index)}
            >
              {choice}
            </span>
          ))}
        </div>
        {type === 'game_type_prompt' && getValueDisplay() && (
          <div className="value-display">
            {getValueDisplay()}
          </div>
        )}
      </div>
    </div>
  );
};

export default PromptModal;

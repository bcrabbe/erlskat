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
      if (type === 'bid_prompt') {
        if (e.key === 'ArrowLeft' || e.key === 'ArrowUp') {
          setSelectedIndex(prev => prev === 0 ? choices.length - 1 : prev - 1);
        } else if (e.key === 'ArrowRight' || e.key === 'ArrowDown') {
          setSelectedIndex(prev => (prev + 1) % choices.length);
        } else if (e.key === 'Enter') {
          handleChoice(choices[selectedIndex]);
        }
      }
    };

    if (isOpen) {
      window.addEventListener('keydown', handleKeyDown);
    }

    return () => {
      window.removeEventListener('keydown', handleKeyDown);
    };
  }, [isOpen, selectedIndex, choices, type]);

  if (type === 'bid_prompt') {
    return (
      <div className="bid-prompt-overlay">
        <div className="bid-prompt-content">
          <div className="turn-indicator">It's your turn to bid</div>
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
              >
                {choice}
              </span>
            ))}
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="modal-overlay" onClick={handleClose}>
      <div className="modal-content" onClick={(e) => e.stopPropagation()}>
        <div className="modal-header">
          <h3>{type === 'card_play_prompt' ? 'Play a Card' : 'Game Prompt'}</h3>
          <button className="close-button" onClick={handleClose}>×</button>
        </div>

        <div className="modal-body">
          <p className="message">{message}</p>

          <div className="choices">
            {choices.map((choice, index) => {
              let valueDisplay = null;
              if (type === 'game_type_prompt' && gameTypeValues.length > 0) {
                const gameTypeValue = gameTypeValues.find(gtv => gtv.game_type === choice);
                valueDisplay = gameTypeValue ? gameTypeValue.value_display : null;
              }

              return (
                <button
                  key={index}
                  className="choice-button"
                  onClick={() => handleChoice(choice)}
                >
                  <div className="choice-content">
                    <span className="choice-main">{typeof choice === 'string' ? choice : choice.toString()}</span>
                    {valueDisplay && (
                      <span className="choice-value">{valueDisplay}</span>
                    )}
                  </div>
                </button>
              );
            })}
          </div>
        </div>
      </div>
    </div>
  );
};

export default PromptModal;

import React from 'react';
import PlayerHand from './PlayerHand';
import UnknownPlayerHand from './UnknownPlayerHand';
import './GameBoard.css';

const GameBoard = ({
  playerHand = [],
  leftPlayerCards = [],
  rightPlayerCards = [],
  leftPlayerCardCount = 0,
  rightPlayerCardCount = 0,
  currentTrick = [],
  onCardClick,
  validCards = [],
  playerId = null,
  tableOrder = [],
  currentBidder = null,
  playerBids = {},
  currentBidValue = 0,
  biddingWinner = null,
  currentCardPlayer = null,
  gameDeclaration = null,
  gameType = null,
  discardPrompt = null,
  selectedDiscardCards = [],
  onDiscardCardClick = null,
  onDiscardSubmit = null,
  cardPlayPrompt = null,
  selectedCardPlay = null,
  onCardPlayClick = null,
  onCardPlaySubmit = null,
  skatCards = [],
  showSkatCards = false,
  hasActiveBidPrompt = false,
  hasInitialChoicePrompt = false,
  gameInfo = null
}) => {
  // Helper function to get player position
  const getPlayerPosition = (targetPlayerId) => {
    if (!playerId || !tableOrder.length) return null;

    const playerIndex = tableOrder.indexOf(playerId);
    const targetIndex = tableOrder.indexOf(targetPlayerId);

    if (playerIndex === -1 || targetIndex === -1) return null;

    // Calculate relative position
    const relativePosition = (targetIndex - playerIndex + tableOrder.length) % tableOrder.length;

    switch (relativePosition) {
      case 1: return 'left';
      case 2: return 'right';
      default: return null; // Current player or invalid
    }
  };

  // Helper function to get player display name
  const getPlayerDisplayName = (targetPlayerId) => {
    const position = getPlayerPosition(targetPlayerId);
    switch (position) {
      case 'left': return 'Left Player';
      case 'right': return 'Right Player';
      default: return 'Unknown Player';
    }
  };

  // Helper function to get bid display text
  const getBidDisplayText = (bidData) => {
    if (!bidData) return '';
    if (bidData.type === 'pass') return `Passed @ ${bidData.value || currentBidValue}`;
    if (bidData.type === 'bid') return `Bid ${bidData.value}`;
    return '';
  };

  // Helper function to get winner display name
  const getWinnerDisplayName = (winnerId) => {
    if (!winnerId || !playerId) return 'Unknown Player';

    if (winnerId === playerId) return 'You';

    const position = getPlayerPosition(winnerId);
    switch (position) {
      case 'left': return 'Left Player';
      case 'right': return 'Right Player';
      default: return 'Unknown Player';
    }
  };

  // Helper function to get player ID by position
  const getPlayerIdByPosition = (position) => {
    if (!playerId || !tableOrder.length) return null;

    const playerIndex = tableOrder.indexOf(playerId);
    if (playerIndex === -1) return null;

    let targetIndex;
    switch (position) {
      case 'left':
        targetIndex = (playerIndex + 1) % tableOrder.length;
        break;
      case 'right':
        targetIndex = (playerIndex + 2) % tableOrder.length;
        break;
      default:
        return null;
    }

    return tableOrder[targetIndex];
  };

  // Helper function to get game declaration display text
  const getGameDeclarationDisplay = (declaration) => {
    if (!declaration) return null;

    const playerName = getWinnerDisplayName(declaration.winnerId);
    const choiceText = declaration.choice === 'hand' ? 'hand' : declaration.choice;

    return {
      playerName,
      choice: choiceText,
      message: declaration.message
    };
  };

  // Helper function to get game type display text
  const getGameTypeDisplay = (gameTypeData) => {
    if (!gameTypeData) return null;

    const playerName = getWinnerDisplayName(gameTypeData.winnerId);
    const gameTypeText = gameTypeData.gameType === 'grand' ? 'Grand Game' :
                        gameTypeData.gameType === 'null' ? 'Null Game' :
                        gameTypeData.gameType === 'ramsch' ? 'Ramsch Game' :
                        gameTypeData.gameType;

    return {
      playerName,
      gameType: gameTypeText,
      message: gameTypeData.message
    };
  };

  // Helper function to get game info display text
  const getGameInfoDisplay = (info) => {
    if (!info) return null;
    
    const gameTypeText = info.gameType === 'grand' ? 'Grand' :
                        info.gameType === 'null' ? 'Null' :
                        info.gameType === 'ramsch' ? 'Ramsch' :
                        info.gameType.charAt(0).toUpperCase() + info.gameType.slice(1);
    
    let displayText;
    
    // Check if there's a declarer (ramsch games don't have declarers)
    if (info.declarer) {
      const declarerName = getWinnerDisplayName(info.declarer);
      const verb = declarerName === 'You' ? 'are' : 'is';
      displayText = `${declarerName} ${verb} playing ${gameTypeText}`;
    } else {
      displayText = `Playing ${gameTypeText}`;
    }
    
    const additionalInfo = [];
    if (info.isHandGame) {
      additionalInfo.push('hand');
    }
    if (info.selectedMultipliers && info.selectedMultipliers.length > 0) {
      additionalInfo.push(...info.selectedMultipliers);
    }
    
    if (additionalInfo.length > 0) {
      displayText += `, ${additionalInfo.join(', ')}`;
    }
    
    return displayText;
  };

  return (
    <div className="game-board">
      {/* Game info display at top */}
      {gameInfo && (
        <div className="game-info-display">
          <div className="game-info-text">
            {getGameInfoDisplay(gameInfo)}
          </div>
        </div>
      )}
      {/* Top player (left) */}
      <div className="player-position left-player">
        <div className="player-info">
          Left Player
          {currentBidder && getPlayerPosition(currentBidder) === 'left' && !hasActiveBidPrompt && (
            <div className="bidding-indicator">
              <div className="thinking-animation">🤔</div>
              <div className="bidding-text">Thinking...</div>
            </div>
          )}
          {currentCardPlayer && getPlayerPosition(currentCardPlayer) === 'left' && (
            <div className="card-play-indicator">
              <div className="thinking-animation">🤔</div>
              <div className="card-play-text">Waiting to play</div>
            </div>
          )}
          {playerBids[getPlayerIdByPosition('left')] && (
            <div className="bid-display">
              {getBidDisplayText(playerBids[getPlayerIdByPosition('left')])}
            </div>
          )}
        </div>
        {leftPlayerCards.length > 0 ? (
          <PlayerHand
            cards={leftPlayerCards}
            isFlipped={true}
          />
        ) : (
          <UnknownPlayerHand
            cardsHeld={leftPlayerCardCount}
            className="left-unknown-hand"
          />
        )}
      </div>

      {/* Center area for current trick */}
      <div className="center-area">
        <div className="current-trick">
          {currentTrick.map((trickCard, index) => (
            <div key={index} className="trick-card">
              <div className="card-rank">{trickCard.card.rank}</div>
              <div className={`card-suit ${trickCard.card.suit === 'hearts' || trickCard.card.suit === 'diamonds' ? 'red' : 'black'}`}>
                {trickCard.card.suit === 'hearts' ? '♥' :
                 trickCard.card.suit === 'diamonds' ? '♦' :
                 trickCard.card.suit === 'clubs' ? '♣' : '♠'}
              </div>
            </div>
          ))}
        </div>

        {/* Bidding status display */}
        {currentBidder && !hasActiveBidPrompt && !biddingWinner && !hasInitialChoicePrompt && (
          <div className="bidding-status">
            <div className="bidding-message">
              Waiting for {getPlayerDisplayName(currentBidder)} to bid...
            </div>
          </div>
        )}

        {/* Bidding winner display with thinking animation */}
        {biddingWinner && (
          <div className="bidding-winner-status">
            <div className="winner-message">
              <div className="winner-announcement">
                🎉 {getWinnerDisplayName(biddingWinner.winnerId)} won the bidding at {biddingWinner.bidValue}!
              </div>
              <div className="thinking-animation-container">
                <div className="thinking-animation">🤔</div>
                <div className="thinking-text">Thinking about game choice...</div>
              </div>
            </div>
          </div>
        )}

        {/* Game declaration display */}
        {gameDeclaration && (
          <div className="game-declaration-status">
            <div className="declaration-message">
              <div className="declaration-announcement">
                {getGameDeclarationDisplay(gameDeclaration).playerName} chose to play {getGameDeclarationDisplay(gameDeclaration).choice}
              </div>
            </div>
          </div>
        )}

        {/* Game type display */}
        {gameType && (
          <div className="game-type-status">
            <div className="type-message">
              <div className="type-announcement">
                {getGameTypeDisplay(gameType).playerName} chose {getGameTypeDisplay(gameType).gameType}
              </div>
            </div>
          </div>
        )}

        {/* Card play waiting status */}
        {currentCardPlayer && !cardPlayPrompt && (
          <div className="card-play-status">
            <div className="card-play-message">
              Waiting for {getPlayerDisplayName(currentCardPlayer)} to play...
            </div>
          </div>
        )}

        {/* Card play prompt for current player */}
        {cardPlayPrompt && (
          <div className="card-play-prompt-status">
            <div className="card-play-prompt-message">
              {cardPlayPrompt.message}
            </div>
          </div>
        )}
      </div>

      {/* Right player */}
      <div className="player-position right-player">
        <div className="player-info">
          Right Player
          {currentBidder && getPlayerPosition(currentBidder) === 'right' && !hasActiveBidPrompt && (
            <div className="bidding-indicator">
              <div className="thinking-animation">🤔</div>
              <div className="bidding-text">Thinking...</div>
            </div>
          )}
          {currentCardPlayer && getPlayerPosition(currentCardPlayer) === 'right' && (
            <div className="card-play-indicator">
              <div className="thinking-animation">🤔</div>
              <div className="card-play-text">Waiting to play</div>
            </div>
          )}
          {playerBids[getPlayerIdByPosition('right')] && (
            <div className="bid-display">
              {getBidDisplayText(playerBids[getPlayerIdByPosition('right')])}
            </div>
          )}
        </div>
        {rightPlayerCards.length > 0 ? (
          <PlayerHand
            cards={rightPlayerCards}
            isFlipped={true}
          />
        ) : (
          <UnknownPlayerHand
            cardsHeld={rightPlayerCardCount}
            className="right-unknown-hand"
          />
        )}
      </div>

      {/* Bottom player (current player) */}
      <div className="player-position bottom-player">
        <div className="player-info">
          Your Hand
          {playerBids[playerId] && (
            <div className="bid-display own-bid">
              {getBidDisplayText(playerBids[playerId])}
            </div>
          )}
        </div>

        {/* Skat cards display */}
        {showSkatCards && skatCards.length > 0 && (
          <div className="skat-cards-display">
            <div className="skat-cards-label">Skat Cards:</div>
            <div className="skat-cards">
              {skatCards.map((card, index) => (
                <div key={index} className="skat-card">
                  <div className="card-rank">{card.rank}</div>
                  <div className={`card-suit ${card.suit === 'hearts' || card.suit === 'diamonds' ? 'red' : 'black'}`}>
                    {card.suit === 'hearts' ? '♥' :
                     card.suit === 'diamonds' ? '♦' :
                     card.suit === 'clubs' ? '♣' : '♠'}
                  </div>
                </div>
              ))}
            </div>
          </div>
        )}

        {/* Discard prompt */}
        {discardPrompt && (
          <div className="discard-prompt">
            <div className="discard-message">{discardPrompt.message}</div>
            {selectedDiscardCards.length === discardPrompt.count && (
              <button
                className="discard-submit-btn"
                onClick={onDiscardSubmit}
              >
                Discard Selected Cards
              </button>
            )}
          </div>
        )}

        {/* Card play prompt */}
        {cardPlayPrompt && (
          <div className="card-play-prompt">
            <div className="card-play-message">{cardPlayPrompt.message}</div>
            {selectedCardPlay !== null && cardPlayPrompt.validCards.includes(selectedCardPlay) && (
              <button
                className="card-play-submit-btn"
                onClick={onCardPlaySubmit}
              >
                Play Selected Card
              </button>
            )}
            <div className="card-play-instruction">
              Use ← → arrows to select, Enter to play
            </div>
          </div>
        )}

        <PlayerHand
          cards={playerHand}
          onCardClick={discardPrompt ? onDiscardCardClick : cardPlayPrompt ? onCardPlayClick : onCardClick}
          validCards={discardPrompt ? playerHand.map((_, index) => index) : cardPlayPrompt ? cardPlayPrompt.validCards : validCards}
          selectedDiscardCards={selectedDiscardCards}
          isDiscardMode={!!discardPrompt}
          selectedCardPlay={selectedCardPlay}
          isCardPlayMode={!!cardPlayPrompt}
        />
      </div>
    </div>
  );
};

export default GameBoard;

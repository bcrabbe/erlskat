const { useInput } = require('ink');
const { useEffect } = require('react');

/**
 * Safe wrapper for useInput that checks for raw mode support
 * Prevents crashes when raw mode is not available
 */
const useSafeInput = (inputHandler) => {
  // Check if raw mode is supported
  const isRawModeSupported = process.stdin.isTTY && process.stdin.setRawMode;
  
  useEffect(() => {
    if (!isRawModeSupported) {
      console.warn('Raw mode not supported, keyboard input disabled');
    }
  }, [isRawModeSupported]);

  // Only use useInput if raw mode is supported
  if (isRawModeSupported) {
    try {
      useInput(inputHandler);
    } catch (error) {
      console.error('Input handler error:', error.message);
    }
  }
};

module.exports = useSafeInput;
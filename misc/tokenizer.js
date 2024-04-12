/**
 * Tokenizes a given string into an array of lexical tokens according to specified patterns.
 * If an unexpected token is found, returns null instead of an array.
 * 
 * @param {string} input The input string to tokenize.
 * @returns {({type: string, value: string}[] | null)} An array of token objects or null if an unexpected token is found.
 */
function tokenize(input) {
  // Define token regex patterns
  const patterns = [
    { type: 'left paren', pattern: /^\(/ },
    { type: 'right paren', pattern: /^\)/ },
    { type: 'dot', pattern: /^\./ },
    { type: 'symbol', pattern: /^[a-zA-Z_][a-zA-Z0-9_]*/ },
    { type: 'unquoted-symbol', pattern: /^,[a-zA-Z_][a-zA-Z0-9_]*/ },
    { type: 'quoted-symbol', pattern: /^'[a-zA-Z_][a-zA-Z0-9_]*/ },
    { type: 'quasiquoted-atom', pattern: /^`[a-zA-Z_][a-zA-Z0-9_]*/ },
    { type: 'quoted left-paren', pattern: /^'\(/ },
    { type: 'quasiquoted left-paren', pattern: /^`\(/ },
    { type: 'unquoted left-paren', pattern: /^,\(/ },
    { type: 'true', pattern: /^#t/ },
    { type: 'false', pattern: /^#f/ },
    { type: 'integer', pattern: /^[0-9]+/ },
    { type: 'whitespace', pattern: /^\s+/ } // Match whitespace but do not return it as a token.
  ];

  let tokens = [];
  let remainingInput = input;
  
  while (remainingInput.length > 0) {
    let matchFound = false;
    for (const {type, pattern} of patterns) {
      const match = remainingInput.match(pattern);
      if (match) {
        if (type !== 'whitespace') { // Ignore whitespace tokens.
          tokens.push({ type: type, value: match[0] });
        }
        remainingInput = remainingInput.slice(match[0].length); // Remove the matched token from the remaining input.
        matchFound = true;
        break;
      }
    }
    if (!matchFound) {
      return null; // Return null if an unexpected token is encountered.
    }
  }

  return tokens;
}

// Example usage:
const input = "(abc def `(,ghi ,jkl))";
console.log(tokenize(input));


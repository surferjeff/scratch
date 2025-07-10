const start = (
    "ABBC" +
    "ABBC" +
    "DEEF" +
    "DGHF" +
    "I..J"
);

function logBoard(board) {
    const chunks = board.match(/.{1,4}/g) || [];
    for (const chunk of chunks)
        console.log(chunk);
}

function enumerateMoves(board) {
    let empty1 = board.indexOf('.');
    let empty2 = board.indexOf('.', empty1 + 1);
    let result = enumerateMovesFrom(board, empty1);
    result.push(...enumerateMovesFrom(board, empty2));
    return result;
}

function enumerateMovesFrom(board, dotIndex) {
    let dotX = dotIndex % 4;
    let result = [];
    if (dotX > 0 && canMove(board, board[dotIndex-1], 1)) {
        result.push(['right', 1, board[dotIndex-1]]);
    }
    if (dotX < 3 && canMove(board, board[dotIndex+1], -1)) {
        result.push(['left', -1, board[dotIndex+1]]);
    }
    if (canMove(board, board[dotIndex - 4], 4)) {
        result.push(['down', 4, board[dotIndex - 4]]);
    }
    if (canMove(board, board[dotIndex + 4], -4)) {
        result.push(['up', -4, board[dotIndex + 4]])
    }
    return result;
}

function canMove(board, letter, step) {
    if (!letter || letter === '.') return false;
    for (let i = board.indexOf(letter); i >= 0; i = board.indexOf(letter, i+1)) {
        let adjacent = board[i + step];
        if (adjacent === letter || adjacent === '.') continue;
        return false;
    }
    return true;
}

function move(board, letter, step) {
    let result = board;
    let indices = [];
    for (let i = board.indexOf(letter); i >= 0; i = board.indexOf(letter, i+1)) {
        indices.push(i);        
    }
    if (step > 0) {
        indices.reverse();
    }
    for (const i of indices) {
        result = swapChars(result, i, i + step);
    }
    return result;
}

function swapChars(str, index1, index2) {
  // Ensure index1 is always the smaller index for consistent slicing
  if (index1 > index2) {
    [index1, index2] = [index2, index1]; // Swap the indices
  }

  const char1 = str[index1];
  const char2 = str[index2];

  // Construct the new string by concatenating parts
  return (
    str.slice(0, index1) +   // Part before the first index
    char2 +                  // Character from the second index
    str.slice(index1 + 1, index2) + // Part between the two indices
    char1 +                  // Character from the first index
    str.slice(index2 + 1)    // Part after the second index
  );
}

const sigLetters = {
    A: 'A',
    C: 'A',
    D: 'A',
    F: 'A',
    B: 'B',
    E: 'E',
    G: 'G',
    H: 'G',
    I: 'G',
    J: 'G',
    '.': '.'
};


function solve(start) {
    let pushStack = [];
    let popStack = [ {board: start }];
    let visited = new Set();
    const limit = 100000;
    for (let n = 0; n < limit; ++n) {
        if (popStack.length === 0) {
            popStack = pushStack.reverse();
            pushStack = [];
        }
        let top = popStack.pop();
        if (!top) {
            throw new Error("No solution!");
        }
        const board = top.board;
        const moves = enumerateMoves(board);
        for (const m of moves) {
            const b = move(board, m[2], m[1]);
            if (/.............BB..BB./.test(b)) {
                return {board: b, move: m, prev: top};
            }
            const signature = b.replace(/./g, match => sigLetters[match]);
            if (!visited.has(signature)) {
                visited.add(signature);
                pushStack.push({board: b, move: m, prev: top});
            }
        }
    }
    throw new Error("Examined", limit, "moves and haven't found a solution!");
}

function nodeMain() {
    const steps = [];
    let step = solve(start);
    while (step) {
        steps.push(step);
        step = step.prev;
    }
    steps.reverse();
    for (const [i, step] of steps.entries()) {
        if (step.move) {
            console.log(String(i).padStart(3, " "), "Move", step.move[2],
                step.move[0])
        }
        logBoard(step.board)
        console.log();
    }
}

function renderBoard(board) {
    let rendered = '';
    for (let y = 0; y < 5; ++y) {
        for (let x = 0; x < 4; ++x) {
            const letter = board[y * 4 + x];
            if (!"ABCDEFGHIJ".includes(letter)) continue;
            if (rendered.includes(letter)) continue;
            const el = document.getElementById(letter);
            el.style.left = `${x * 100 + 5}px`;
            el.style.top = `${y * 100 + 5}px`;
            rendered = rendered + letter;
        }
    }
}

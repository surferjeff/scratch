

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

// logBoard(start);
// const moves = enumerateMoves(start);
// console.log(moves);
// let m = moves[0];
// let b = move(start, m[2], m[1]);
// logBoard(b)

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
    let popStack = [ start ];
    let visited = new Set();
    for (let n = 0; n < 10000000; ++n) {
        if (popStack.length === 0) {
            popStack = pushStack.reverse();
            pushStack = [];
        }
        let board = popStack.pop();
        if (!board) {
            throw new Error("No solution!");
        }
        const moves = enumerateMoves(board);
        // console.log("============");
        // logBoard(board);
        // console.log(moves);
        for (const m of moves) {
            const b = move(board, m[2], m[1]);
            if (/.............BB..BB./.test(b)) {
                logBoard(b);
                console.log("Solved!");
                return;
            }
            const signature = b.replace(/./.g, match => sigLetters[match]);
            if (!visited.has(signature)) {
                visited.add(signature);
                pushStack.push(b);
                // console.log(m);
                // logBoard(b);
                // console.log("");
            }
        }
    }   
}

solve(start);

// const test =
//     "A.BB" +
//     "AFBB" +
//     "EEEE";
// console.log(enumerateMoves(test));
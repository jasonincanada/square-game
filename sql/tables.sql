CREATE TABLE IF NOT EXISTS board (
 boardID     INTEGER PRIMARY KEY,
 sequence    TEXT    NOT NULL
);

CREATE TABLE IF NOT EXISTS boardSquare (
  fkBoardID  INTEGER NOT NULL,
  squareRow  INTEGER NOT NULL,
  squareCol  INTEGER NOT NULL,
  squareSize INTEGER NOT NULL,

  FOREIGN KEY (fkBoardID) REFERENCES board(boardID)
);

CREATE TABLE IF NOT EXISTS player (
  playerID       INTEGER PRIMARY KEY AUTOINCREMENT,
  dateRegistered INTEGER NOT NULL,
  name           TEXT
);

CREATE TABLE IF NOT EXISTS game (
  gameID       INTEGER PRIMARY KEY AUTOINCREMENT,
  dateStarted  INTEGER NOT NULL,
  dateFinished INTEGER NULL,
  fkBoardID    INTEGER NOT NULL,
  fkPlayerID   INTEGER NOT NULL,

  FOREIGN KEY (fkBoardID)  REFERENCES board(boardID),
  FOREIGN KEY (fkPlayerID) REFERENCES player(playerID)
);

CREATE TABLE IF NOT EXISTS shroud (
  fkGameID     INTEGER NOT NULL,
  gridRow      INTEGER NOT NULL,
  gridCol      INTEGER NOT NULL,

  FOREIGN KEY (fkGameID)  REFERENCES game(gameID)
);

CREATE TABLE IF NOT EXISTS placedSquare (
  fkGameID     INTEGER NOT NULL,
  squareRow    INTEGER NOT NULL,
  squareCol    INTEGER NOT NULL,
  squareSize   INTEGER NOT NULL,

  FOREIGN KEY (fkGameID)  REFERENCES game(gameID)
);


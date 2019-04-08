/* Generate all valid Partridge Squares for any N */

// #define N8 - 14 hr 2 min
// #define N7 - 1 min 22 sec
#define N3

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace jrh.PartridgeSquares.Generation
{
    // The placement of a square at a row and column
    public class Placement
    {
        public int Square { get; private set; }
        public int Row { get; private set; }
        public int Column { get; private set; }

        public Placement(int square, int row, int column)
        {
            Square = square;
            Row = row;
            Column = column;
        }
    }

    // The main board to tile with sub-squares. This is a highly mutable structure with fixed array
    // sizes for the column levels and a variable-sized stack for the list of currently placed squares
    public class Board
    {
        public static int N { get; private set; }
        public static int Side { get; private set; }

        // Measure how long the run takes
        private DateTime start;
        private DateTime end;

        // Side+1 because we add on a side-length wall at the very right
#if N8
        private int[] levels = new int[8 * (8 + 1) / 2 + 1];
#elif N7
        private int[] levels = new int[7 * (7 + 1) / 2 + 1];
#else
        private int[] levels = new int[3 * (3 + 1) / 2 + 1];
#endif        

        // n+1 because squares is 1-based for simplicity
#if N8
        private int[] squares = new int[8 + 1];
#elif N7
        private int[] squares = new int[7 + 1];
#else
        private int[] squares = new int[3 + 1];
#endif        

        private int depth;
        private int logLevel;

        // Stack of squares placed
        public Stack<Placement> placements { get; private set; }

        public Board(int n, int log)
        {
            N = n;
            Side = N * (N + 1) / 2;
            logLevel = log;

            depth = 0;
            placements = new Stack<Placement>();

            // Generate our starting set of squares (keep track of the number of each left)
            for (int s = 1; s <= n; s++)
                squares[s] = s;

            // Put a wall on the far right
            levels[Side] = Side;
        }

        void Place(int square, int row, int column)
        {
            placements.Push(new Placement(square, row, column));
            depth++;

            if (depth == logLevel)
                Progress();

            // Update column levels
            for (int i = 0; i < square; i++)
                levels[column + i] += square;

            // We now have one fewer of these squares to place
            squares[square]--;
        }

        // The inverse operation to Place
        void Unplace()
        {
            Placement p = placements.Pop();
            depth--;

            // Update column levels
            for (int i = 0, column = p.Column; i < p.Square; i++)
                levels[column + i] -= p.Square;

            // We now have one more of these squares to place
            squares[p.Square]++;
        }

        // Try all squares that fit at this upper-left corner
        void TrySquaresAt(int row, int column)
        {
            int level = levels[column];
            int space = 0;

            int nextRow, nextColumn;

            // Measure space available until we run into a wall
            for (int i = column; levels[i] == level; i++)
            {
                space++;

                // No sense measuring past the point of the biggest square
                if (space == N)
                    break;
            }

            // Constrain to fit vertically on the game board
            space = Math.Min(space, Side - levels[column]);

            // Try the largest fitting square first and work downwards to 1
            for (int square = space; square >= 1; square--)
            {
                if (squares[square] == 0)
                    continue;

                Place(square, row, column);

                // Have we placed all the squares?
                if (depth == Side)
                {
                    Visit(placements);

                    Console.WriteLine("Found square boom!");
                }
                else
                {
                    FindNextPlacementPoint(out nextRow, out nextColumn);

                    // The recursive step, we still have squares left, so try the next one
                    TrySquaresAt(nextRow, nextColumn);
                }

                Unplace();
            }
        }

        // Get the next place to put a square (the "top-left-most" empty tile)
        void FindNextPlacementPoint(out int row, out int column)
        {
            row = Side;
            column = 0;

            for (int i = 0; i < Side; i++)
            {
                if (levels[i] < row)
                {
                    row = levels[i];
                    column = i;
                }
            }
        }

        private void Visit(IEnumerable<Placement> placements)
        {
            // Write this particular solution to its own file
            using (var file = new StreamWriter(GetFileName()))
            {
                // Write the individual sub-square size/coordinates
                foreach (var s in placements.Reverse())
                    file.WriteLine("{0} {1},{2}", s.Square, s.Row, s.Column);

                file.WriteLine("");

                // Render an ASCII version of the board right in the file
                var grid = new Dictionary<Tuple<int, int>, int>();

                foreach (var p in placements)
                {
                    for (var row = p.Row; row < p.Row + p.Square; row++)
                        for (var column = p.Column; column < p.Column + p.Square; column++)
                            grid.Add(new Tuple<int, int>(row, column), p.Square);
                }

                for (var row = 0; row < Side; row++)
                {
                    var line = "";

                    for (var column = 0; column < Side; column++)
                    {
                        var t = new Tuple<int, int>(row, column);

                        if (grid.ContainsKey(t))
                            line += grid[t];
                        else
                            line += "-";
                    }

                    file.WriteLine(line);
                }
            }
        }

        // A board's filename is the board size plus the square sizes in order of their
        // top-down/left-right placement, this uniquely identifies a Partridge Square
        private string GetFileName()
        {
            string prefix = string.Format("squares/N{0}-", N);
            string extension = ".sqr";

            var components = placements
                .Reverse()
                .Select(p => p.Square.ToString())
                .ToList();

            return prefix + string.Join("", components) + extension;
        }

        // Periodically output a progress report
        private void Progress()
        {
            Console.Write("Level {0}: [ ", depth);

            foreach (var p in placements.Reverse())
                Console.Write("{0} ", p.Square);

            Console.WriteLine("]");
        }

        // The main method that kicks off the search
        public void Search()
        {
            start = DateTime.UtcNow;
            TrySquaresAt(0, 0);
            end = DateTime.UtcNow;

            Console.WriteLine("Started {0}, finished {1}", start, end);
        }
    }


    class Program
    {
        static void Main(string[] args)
        {
#if N8
            var board = new Board(8, 10);
#elif N7
            var board = new Board(7, 6);
#else
            var board = new Board(3, 3);
#endif

            board.Search();
        }
    }
}

// n=8:
// Started 4/07/19 10:19:17 PM, finished 4/08/19 12:21:58 PM
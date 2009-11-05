package jw.othello.test;

import jw.othello.client.Board;

public class Test01
{
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        Board board = new Board();
        board.setColor1("00ff00");
        board.setColor2("0077ff");
        board.cellAt(3, 3).setValue(1);
        board.cellAt(4, 4).setValue(1);
        board.cellAt(3, 4).setValue(2);
        board.cellAt(4, 3).setValue(2);
    }
    
}

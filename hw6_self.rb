# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here

  # your enhancements here
  All_My_Pieces = Piece::All_Pieces + [
  	rotations([[0,0],[1,0],[0,1],[1,1],[2,1]]),
  	[[[0,0],[0,-1],[0,-2],[0,1],[0,2]],
  	 [[0,0],[-1,0],[-2,0],[1,0],[2,0]]],
  	rotations([[0,0],[-1,0],[0,1]])
  ]
  def self.next_piece(board, cheat = false)
  	if cheat then
  		MyPiece.new([[[0,0]]],board)
  	else 
  		MyPiece.new(All_My_Pieces.sample,board)
  	end
  end

  
end

class MyBoard < Board
  # your enhancements here
  def initialize(game)
  	super
  	@cheat = false
  	@current_block = MyPiece.next_piece(self)
  end

  def next_piece
    @current_block = MyPiece.next_piece(self,@cheat)
    @current_pos = nil
    @cheat = false
  end

  def rotate180
  	rotate_clockwise
  	rotate_clockwise
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.size - 1).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def cheat_action
  	if not @cheat and @score >= 100 then
  		@cheat = true
  		@score -= 100
  	end
  	draw
  end

end

class MyTetris < Tetris
  # your enhancements here
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
  def key_bindings
    super
  	@root.bind('u',proc {@board.rotate180})
  	@root.bind('c',proc {@board.cheat_action})
  end
end


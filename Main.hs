import Graphics.Gloss.Interface.Pure.Game
import Data.Array
import Menu
data Player = Pl_X|Pl_O|Wait deriving (Eq,Show)
data State= Play|WaitMenu|Bot|GameOver (Maybe Player) deriving (Eq,Show)
data Cell = Empty|Full Player|String deriving (Eq,Show)
type Board = Array (Int,Int) Cell 


data Game = Game{gameField::Board
                ,gamePlayer::Player
                ,gameStatus::State
                } deriving (Eq,Show)

main::IO()
main = play window bgColor 60 initMenu gamePicture gamePlay updateGame
  where
    window = InWindow "Game" (screenWidth, screenHeight) (400, 200)
    bgColor = black  -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

initGame = Game {gameField = array indexRange $ zip (range indexRange) (cycle [Empty])
                ,gamePlayer = Pl_X
                ,gameStatus = Play
                }
     where
       indexRange = ((0,0),(boardWidth - 1,boardHeight - 1))

initBot = Game {gameField = array indexRange $ zip (range indexRange) (cycle [Empty])
                ,gamePlayer = Pl_X
                ,gameStatus = Bot
                }
     where
       indexRange = ((0,0),(boardWidth - 1,boardHeight - 1))          

initMenu = Game {gameField = array indexRange $ zip (range indexRange) (cycle [Empty])
                ,gamePlayer = Wait
                ,gameStatus = WaitMenu
                }
                where
                  indexRange = ((0,0),(1,1))           



plXcolor = orange
plOcolor = azure
boardGridColor = white
tieColor = magenta
w = cyan


outColor (Just Pl_X) = plXcolor
outColor (Just Pl_O) = plOcolor
outColor Nothing = tieColor

snapPictureToCell picture (row,column) = translate x y picture
   where
      x = fromIntegral column*cellWidth+cellWidth*0.5
      y = fromIntegral row*cellHeigth + cellHeigth*0.5

cellsofBoard::Board->Cell->Picture->Picture
cellsofBoard board cell cellPicture = 
  pictures
    $ map (snapPictureToCell cellPicture.fst)--берет первый элемент кортежа 
    $ filter(\(_,e)->e == cell)
    $ assocs board

xCell :: Picture
xCell = pictures [rotate 45.0 $ rectangleSolid side 10.0
        , rotate (-45.0) $ rectangleSolid side 10.0]
        where
         side = min cellWidth cellHeigth*0.75
  

-- | Нарисовать «нолик».
oCell :: Picture
oCell = thickCircle radius 10.0
       where
        radius = min cellWidth cellHeigth*0.25

xCellsofBoard::Board->Picture
xCellsofBoard board = cellsofBoard board (Full Pl_X) xCell 

oCellsofBoard::Board->Picture
oCellsofBoard board = cellsofBoard board (Full Pl_O) oCell

boardGrid :: Picture
boardGrid = pictures $ concatMap (\i->[line[(i*cellWidth,0.0)
                                        ,(i*cellWidth,fromIntegral screenHeight)
                                         ]
                                    ,line [(0.0,i*cellHeigth)
                                          ,(fromIntegral screenWidth,i*cellHeigth)
                                            ]
                                        ])
                       [0.0..fromIntegral boardWidth]

boardPlayPic board = 
  pictures [color plXcolor $ xCellsofBoard board
           ,color plOcolor $ oCellsofBoard board
           ,color boardGridColor $ boardGrid
           ]

boardPicture board = 
  pictures [xCellsofBoard board
           ,oCellsofBoard board
           ,boardGrid
           ]
boardWinPic winner board = pictures [color w (choseWinner winner)]           
  where
     choseWinner win = case win of 
                     (Just Pl_X) -> xCellsofBoard board 
                     (Just Pl_O) -> oCellsofBoard board
                     (Nothing) -> boardEndPic winner board

boardEndPic winner board = color (outColor winner) (boardPicture board)

gamePicture::Game -> Picture
gamePicture game = translate (fromIntegral screenWidth*(-0.5))
                             (fromIntegral screenHeight*(-0.5)) 
                             frame
 where
  frame = case gameStatus game of 
                Play -> boardPlayPic (gameField game)
                GameOver winner -> boardWinPic winner (gameField game) --boardEndPic winner (gameField game)
                WaitMenu -> menuPlayPic game
                Bot ->boardPlayPic (gameField game)



correctCor = inRange ((0,0),(boardWidth -1,boardWidth-1))


buttomPosition::(Float,Float) -> (Int,Int)
buttomPosition (x,y) =(floor y, floor x) 
buttomCord::Game->(Int,Int)->Game
buttomCord game x | (fst x) >= 15 && (fst x) <= 150 && (snd x) >= -210 && (snd x) <= 210 = initGame
                  | (fst x) >= -190 && (fst x) <= -50 && (snd x) >= -210 && (snd x) <= 210 = initBot
                  | otherwise = game

                 

switchPlayer game = 
  case gamePlayer game of
    Pl_X -> game {gamePlayer = Pl_O}
    Pl_O -> game {gamePlayer = Pl_X}

playerWon::Player->Board->Bool
playerWon player board = any isVictorProj allcords
  where 
    allcords = allrowCoords ++ allColumsCoords ++ allDiagCoords
    allrowCoords = [[(i,j) | j<-[0..boardWidth-1]] | i<-[0..boardWidth-1]]
    allColumsCoords = [[(j,i)| j <- [0..boardWidth-1]]|i<-[0..boardWidth-1]]
    allDiagCoords = [[(i,i)|i <- [0..boardWidth-1]],[(i,boardWidth-1-i)|i<-[0..boardWidth-1]]]
    isVictorProj allcords = (boardWidth ==) $ length $ filter(\cell->cell==Full player) $ map (\coord->board ! coord) allcords



countCells::Cell->Board -> Int
countCells cell = length.filter ((==) cell).elems

checkGameOver game | playerWon Pl_X board = game {gameStatus = GameOver $ Just Pl_X}
                   | playerWon Pl_O board = game {gameStatus = GameOver $ Just Pl_O}
                   | countCells Empty board == 0 = game {gameStatus = GameOver Nothing}
                   | otherwise = game
                    where board = gameField game 
                   
playerTurn::Game -> (Int,Int)->Game
playerTurn game cellCoord
                         |correctCor cellCoord && board ! cellCoord == Empty = checkGameOver $ switchPlayer $ game {gameField = board // [(cellCoord,Full $ player)]} 
                         |otherwise = game 
                          where
                           board = gameField game
                           player = gamePlayer game


playerTurnBot::Game -> (Int,Int)->Game
playerTurnBot game cellCoord
                         |correctCor cellCoord && board ! cellCoord == Empty = botTurn $ checkGameOver $ switchPlayer $ game {gameField = board // [(cellCoord,Full $ player)]} 
                         |otherwise = game 
                          where
                           board = gameField game
                           player = gamePlayer game

                           
checkBot::Game->Bool
checkBot  game | countCells Empty board == 0 = False
               | otherwise = True
                  where
                   board = gameField game                     

botTurn::Game->Game
botTurn game        |checkBot game == True && gameField game ! (0,0) /= Empty && gameField game ! (2,2) == Empty = checkGameOver $ switchPlayer $ game {gameField = board // [((2,2),Full $ player)]}
                    |checkBot game == True && gameField game ! (0,0) /= Empty && gameField game ! (2,0) == Empty  = checkGameOver $ switchPlayer $ game {gameField = board // [((2,0),Full $ player)]}
                    |checkBot game == True && gameField game ! (0,0) /= Empty && gameField game ! (1,0) == Empty  = checkGameOver $ switchPlayer $ game {gameField = board // [((1,0),Full $ player)]}
                    |checkBot game == True && gameField game ! (0,0) /= Empty && gameField game ! (1,1) == Empty  = checkGameOver $ switchPlayer $ game {gameField = board // [((1,1),Full $ player)]}
                    |checkBot game == True && gameField game ! (0,0) /= Empty && gameField game ! (0,2) == Empty  = checkGameOver $ switchPlayer $ game {gameField = board // [((0,2),Full $ player)]}
                    |checkBot game == True && gameField game ! (0,0) /= Empty && gameField game ! (0,1) == Empty  = checkGameOver $ switchPlayer $ game {gameField = board // [((0,1),Full $ player)]}
                    |checkBot game == True && gameField game ! (0,1) /= Empty && gameField game ! (0,0) == Empty  = checkGameOver $ switchPlayer $ game {gameField = board // [((0,0),Full $ player)]}
                    |checkBot game == True && gameField game ! (0,1) /= Empty && gameField game ! (2,1) == Empty  = checkGameOver $ switchPlayer $ game {gameField = board // [((2,1),Full $ player)]}
                    |checkBot game == True && gameField game ! (0,1) /= Empty && gameField game ! (1,1) == Empty  = checkGameOver $ switchPlayer $ game {gameField = board // [((1,1),Full $ player)]}
                    |checkBot game == True && gameField game ! (0,2) /= Empty && gameField game ! (2,0) == Empty  = checkGameOver $ switchPlayer $ game {gameField = board // [((2,0),Full $ player)]}
                    |checkBot game == True && gameField game ! (0,2) /= Empty && gameField game ! (1,1) == Empty  = checkGameOver $ switchPlayer $ game {gameField = board // [((1,1),Full $ player)]}
                    |checkBot game == True && gameField game ! (0,2) /= Empty && gameField game ! (1,2) == Empty  = checkGameOver $ switchPlayer $ game {gameField = board // [((1,2),Full $ player)]}
                    |checkBot game == True && gameField game ! (0,2) /= Empty && gameField game ! (2,2) == Empty  = checkGameOver $ switchPlayer $ game {gameField = board // [((2,2),Full $ player)]}
                    |checkBot game == True && gameField game ! (1,0) /= Empty && gameField game ! (1,2) == Empty  = checkGameOver $ switchPlayer $ game {gameField = board // [((1,1),Full $ player)]}
                    |checkBot game == True && gameField game ! (1,0) /= Empty && gameField game ! (0,0) == Empty  = checkGameOver $ switchPlayer $ game {gameField = board // [((0,0),Full $ player)]}
                    |checkBot game == True && gameField game ! (1,1) /= Empty && gameField game ! (0,2) == Empty  = checkGameOver $ switchPlayer $ game {gameField = board // [((0,2),Full $ player)]}
                    |checkBot game == True && gameField game ! (1,1) /= Empty && gameField game ! (1,0) == Empty  = checkGameOver $ switchPlayer $ game {gameField = board // [((1,0),Full $ player)]}
                    |checkBot game == True && gameField game ! (1,1) /= Empty && gameField game ! (0,0) == Empty  = checkGameOver $ switchPlayer $ game {gameField = board // [((0,0),Full $ player)]}
                    |checkBot game == True && gameField game ! (1,1) /= Empty && gameField game ! (0,1) == Empty  = checkGameOver $ switchPlayer $ game {gameField = board // [((0,1),Full $ player)]}
                    |checkBot game == True && gameField game ! (1,2) /= Empty && gameField game ! (0,2) == Empty  = checkGameOver $ switchPlayer $ game {gameField = board // [((0,2),Full $ player)]}
                    |checkBot game == True && gameField game ! (2,0) /= Empty && gameField game ! (2,2) == Empty  = checkGameOver $ switchPlayer $ game {gameField = board // [((2,2),Full $ player)]}
                    |checkBot game == True && gameField game ! (2,0) /= Empty && gameField game ! (2,1) == Empty  = checkGameOver $ switchPlayer $ game {gameField = board // [((2,1),Full $ player)]}
                    |checkBot game == True && gameField game ! (2,1) /= Empty && gameField game ! (2,0) == Empty  = checkGameOver $ switchPlayer $ game {gameField = board // [((2,0),Full $ player)]}
                    |checkBot game == True && gameField game ! (0,0) == Empty = checkGameOver $ switchPlayer $ game {gameField = board // [((0,0),Full $ player)]}  
                    |checkBot game == True && gameField game ! (0,2) == Empty = checkGameOver $ switchPlayer $ game {gameField = board // [((0,2),Full $ player)]}
                    |checkBot game == True && gameField game ! (1,1) == Empty = checkGameOver $ switchPlayer $ game {gameField = board // [((1,1),Full $ player)]}
                    |checkBot game == True && gameField game ! (1,1) /= Empty && gameField game  ! (0,1) == Empty = checkGameOver $ switchPlayer $ game {gameField = board // [((1,1),Full $ player)]}
                    |checkBot game == True && gameField game ! (1,1) /= Empty && gameField game  ! (2,1) == Empty = checkGameOver $ switchPlayer $ game {gameField = board // [((2,1),Full $ player)]}
                    |checkBot game == True && gameField game ! (1,2) == Empty = checkGameOver $ switchPlayer $ game {gameField = board // [((1,2),Full $ player)]} 
                    |checkBot game == True && gameField game ! (2,2) == Empty = checkGameOver $ switchPlayer $ game {gameField = board // [((2,2),Full $ player)]} 
                    |otherwise = game
                    where
                     board = gameField game
                     player = gamePlayer game


mousePosition::(Float,Float) -> (Int,Int)
mousePosition (x,y) = (floor ((y + (fromIntegral screenHeight*0.5))/cellHeigth), floor((x + (fromIntegral screenWidth*0.5))/cellWidth))




gamePlay :: Event -> Game -> Game
gamePlay (EventKey (MouseButton LeftButton) Down _ mouse) game =
  case gameStatus game of
        Play -> playerTurn game $ mousePosition mouse  
        GameOver _ -> initGame
        WaitMenu -> buttomCord game $  buttomPosition mouse
        Bot -> playerTurnBot game $ mousePosition mouse

gamePlay _ game =  game




boardWidth :: Int
boardWidth  = 3

-- | Высота игрового поля в клетках.
boardHeight :: Int
boardHeight = 3


-- | Ширина экрана в пикселях.
screenWidth :: Int
screenWidth  = 720

-- | Высота экрана в пикселях.
screenHeight :: Int
screenHeight = 620    

-- | Размер одной клетки в пикселях.
cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral boardWidth 

cellHeigth :: Float
cellHeigth = fromIntegral screenHeight / fromIntegral boardWidth 



updateGame :: Float -> Game -> Game
updateGame _ = id
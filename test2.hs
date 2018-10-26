import Data.Map
data ClassBand = Class String String String String deriving Show
claSS = Class  "Caliban" "Slipknot" "AMATORY" "Heart Of A Coward"
data Oneman = Oneman String deriving Show
oneM =  Oneman "OnlyOne"

data Person = Person String String Unit deriving Show
me = Person "Se" "Na" OnlyOne
data Noperson = Noperson String String deriving Show
noMe = Noperson "Grisha" "HeH"

type Song = String


bands = [Caliban, Slipknot , AMATORY , HeartOfACoward,OnlyOne]
songs::[Song]
songs = ["No Tomorrow","Wait And Bleed","On The Brink","Mourning Repairs","LoLKeKCHEBYREK"]
names = ["Gortz,Dorner, Smidt,Schaller,Grun", "Shawn Crahan, Craig Jones, Mick Thomson, Corey Taylor, Sid Wilson, Chris Fehn, Jim Root, Alessandro Venturella, JayWeinberg","Denis Zhivotovskiy, Daniil Svetlov, Ilya Borisov, Dmitriy Muzychenko","Stiv Haikok, Criss Mansbridg, Wishal Cayt, Carl Isek, Kain Tasan","Vasiy Kek"    ]     
bestSongs1 = bands `zip` songs
members = bands `zip` names

data Unit = Caliban | Slipknot|AMATORY| HeartOfACoward|OnlyOne deriving (Show,Ord,Eq)
move:: Unit-> String
move Caliban = "Gortz,Dorner, Smidt,Schaller,Grun" 
move Slipknot = "Shawn Crahan, Craig Jones, Mick Thomson, Corey Taylor, Sid Wilson, Chris Fehn, Jim Root, Alessandro Venturella, JayWeinberg"
move AMATORY  = "Denis Zhivotovskiy, Daniil Svetlov, Ilya Borisov, Dmitriy Muzychenko"
move HeartOfACoward  = "Stiv Haikok, Criss Mansbridg, Wishal Cayt, Carl Isek, Kain Tasan"
move OnlyOne  = "Vasiy Kek"

class User man where 
 getMem::man->String
 getRS::man->Maybe Song
 getFB:: man->Unit

instance User Person where
 getMem (Person _  _ band) = move band
 getFB  (Person _ _ band) = band
 getRS person = getBestSong (getFB person) bestSongs
                              where bestSongs = Data.Map.fromList bestSongs1 
                                    getBestSong bands songs = Data.Map.lookup bands songs

instance User Noperson where 
  getMem (Noperson _ _) = error "KYPU PODPISKY"
  getRS (Noperson _ _)  = error "KYPU PODPISKY"
  getFB (Noperson _ _) = error "KYPU PODPISKY"

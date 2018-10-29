import Data.Map
data ClassBand = Class String String String String deriving Show
claSS = Class  "Caliban" "Slipknot" "AMATORY" "Heart Of A Coward"
data Oneman = Oneman String deriving Show
oneM =  Oneman "OnlyOne"

data Man = Person String String Band | Noperson String String|Bandmem String String deriving Show
me = Person "Se" "Na" OnlyOne
noMe = Noperson "Grisha" "HeH"

type Song = String


bands = [Caliban, Slipknot , AMATORY , HeartOfACoward,OnlyOne]
songs::[Song]
songs = ["No Tomorrow","Wait And Bleed","On The Brink","Mourning Repairs","LoLKeKCHEBYREK"]
names = ["Gortz,Dorner, Smidt,Schaller,Grun", "Shawn Crahan, Craig Jones, Mick Thomson, Corey Taylor, Sid Wilson, Chris Fehn, Jim Root, Alessandro Venturella, JayWeinberg","Denis Zhivotovskiy, Daniil Svetlov, Ilya Borisov, Dmitriy Muzychenko","Stiv Haikok, Criss Mansbridg, Wishal Cayt, Carl Isek, Kain Tasan","Vasiy Kek"    ]     
bestSongs1 = bands `zip` songs
members = bands `zip` names

data Band = Caliban | Slipknot|AMATORY| HeartOfACoward|OnlyOne deriving (Show,Ord,Eq)
move:: Band-> String
move Caliban = "Gortz,Dorner, Smidt,Schaller,Grun" 
move Slipknot = "Shawn Crahan, Craig Jones, Mick Thomson, Corey Taylor, Sid Wilson, Chris Fehn, Jim Root, Alessandro Venturella, JayWeinberg"
move AMATORY  = "Denis Zhivotovskiy, Daniil Svetlov, Ilya Borisov, Dmitriy Muzychenko"
move HeartOfACoward  = "Stiv Haikok, Criss Mansbridg, Wishal Cayt, Carl Isek, Kain Tasan"
move OnlyOne  = "Vasiy Kek"

getMem::Man->String
getMem (Person _  _ band) = move band
getMem (Noperson _ _) = error "KYPU PODPISKY"

getFB::Man->Band
getFB  (Person _ _ band) = band
getFB (Noperson _ _) = error "KYPU PODPISKY"

getRS::Man->Maybe Song
getRS (Noperson _ _) = error "KYPU PODPISKY"
getRS (Person a b band) = getBestSong (getFB (Person a b band)) bestSongs
                            where bestSongs = Data.Map.fromList bestSongs1 
                                  getBestSong bands songs = Data.Map.lookup bands songs
                                                         




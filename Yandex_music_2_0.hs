
data MusicBand = Caliban | Slipknot | AMATORY | Heart_Of_A_Coward | OnlyOne deriving (Show,Eq,Ord)

data ClassBand = Class String String String String deriving Show
claSS = Class  "Caliban" "Slipknot" "AMATORY" "Heart Of A Coward"

data Oneman = Oneman String deriving Show
oneM =  Oneman "OnlyOne"


data Person = Person String String MusicBand deriving Show 
data Noperson = Noperson String String deriving Show {-Не авторизованные люди -}

type Song = String

me =  Person "Se" "Na" Caliban  
nome = Noperson "Grisha" "HeH"

getFB:: Person->MusicBand
getFB  (Person _ _ band) = band 

names = ["Gortz,Dorner, Smidt,Schaller,Grun", "Shawn Crahan, Craig Jones, Mick Thomson, Corey Taylor, Sid Wilson, Chris Fehn, Jim Root, Alessandro Venturella, JayWeinberg","Denis Zhivotovskiy, Daniil Svetlov, Ilya Borisov, Dmitriy Muzychenko","Stiv Haikok, Criss Mansbridg, Wishal Cayt, Carl Isek, Kain Tasan","Vasiy Kek"    ]     
bands = ["Caliban", "Slipknot" , "AMATORY" , "Heart Of A Coward","OnlyOne"]
songs::[Song]
songs = ["No Tomorrow","Wait And Bleed","On The Brink","Mourning Repairs","LoLKeKCHEBYREK"]
bestSongs = bands `zip` songs
members = bands `zip` names

get::(Eq a) =>[(a,b)]->a->b  
get ((key,value):xs) k = if (k == key) then value else get xs k
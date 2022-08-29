main :: IO ()
main = putStrLn "hello world"

data Item = Sword | Bow | MagicWand | Maybe deriving (Eq, Read)
data Mob = Mummy | Skeleton Item | Witch (Maybe Item) deriving (Eq)

instance Show Item where
    show Sword = "sword"
    show Bow = "bow"
    show MagicWand = "magic wand"

createMummy :: Mob
createMummy = Mummy

createArcher :: Mob
createArcher = Skeleton Bow

createKnight :: Mob
createKnight = Skeleton Sword

createWitch :: Mob
createWitch = Witch Nothing

createSorceress :: Mob
createSorceress = Witch (Just MagicWand)

create :: String -> Maybe Mob
create "mummy" = Just createMummy
create "doomed archer" = Just createArcher
create "dead knight" = Just createKnight
create "witch" = Just createWitch
create "sorceress" = Just createSorceress
create _ = Nothing

equip :: Item -> Mob -> Maybe Mob
equip item (Skeleton other) = Just (Skeleton item)
equip item (Witch (Just other)) = Just (Witch (Just item))
equip item (Witch Nothing) = Just (Witch (Just item))
equip item Mummy = Nothing

instance Show Mob where
    show Mummy = "mummy"
    show (Skeleton Bow) = "doomed archer"
    show (Skeleton Sword) = "dead knight"
    show (Skeleton MagicWand) = "skeleton holding a magicWand"
    show (Witch Nothing) = "which"
    show (Witch (Just MagicWand)) = "sorceress"
    show (Witch (Just Sword)) = "witch holding a sword"
    show (Witch (Just Bow)) = "witch holding a bow"

class HasItem obj where
    getItem :: obj -> Maybe Item
    hasItem :: obj -> Bool

instance HasItem Mob where
    getItem Mummy = Nothing
    getItem (Skeleton Sword) = Just Sword
    getItem (Skeleton Bow) = Just Bow
    getItem (Skeleton MagicWand) = Just MagicWand
    getItem (Witch (Just Sword)) = Just Sword
    getItem (Witch (Just Bow)) = Just Bow
    getItem (Witch (Just MagicWand)) = Just MagicWand
    getItem (Witch Nothing) = Nothing

    hasItem Mummy = False
    hasItem (Skeleton Sword) = True
    hasItem (Skeleton Bow) = True
    hasItem (Skeleton MagicWand) = True
    hasItem (Witch (Just Sword)) = True
    hasItem (Witch (Just Bow)) = True
    hasItem (Witch (Just MagicWand)) = True
    hasItem (Witch Nothing) = False
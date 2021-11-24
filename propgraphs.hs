import System.IO (char8, utf8)
import Control.Monad (when)
import Data.Char (toLower)
import Data.List ()
import Data.Maybe ( fromJust, isJust, isNothing )
import Data.Either ( fromLeft, isLeft, fromRight )
import Data.Text.Internal.Unsafe (inlinePerformIO)
{-
########################## LLENGUATGES DE PROGRAMACIÓ ########################
                            · Group:      11                               
                            · Student:    Pau Vallespí                       
                            · Professor:  Jordi Petit                       
##############################################################################


################################ DEFINITIONS ################################

A property graph G is a tuple (V, E, ρ, λ, σ):
V: is a finite set of vertices (or nodes).

E: is a finite set of edges such that V and E have no elements in common.

ρ: is a total function. Intuitively 
    ρ(e) = (v1, v2) indicates that e is a directed edge from node v1 to node v2 in G.

λ: is a total function with Lab a set of labels. Intuitively,
    (a) If v ∈ V and λ(v) = l, then l is the label of node v in G. 
    (b) If e ∈ E and λ(e) = l, then l is the label of edge e in G.

σ: is a partial function with Prop a finite set of properties and V al a set of values.
    (a) If v ∈ V, p ∈ Prop and σ(v,p) = s, then s is the value of property p for 
    node v in the property graph G.
    (b) If e ∈ E, p ∈ Prop and σ(e,p) = s, then s is the value of property p for 
    edge e in the property graph G.
    Must be defined in terms of the basic types:
        · Integer
        · Double
        · String
        · Boolean
        · Date

#############################################################################



################################ DECISIONS ################################

1. A Graph will be represented by a list of nodes, edges and properties.

2. Each node is composed of a string, which represents its name, a label that
   the user can define, and a list of properties.

3. Each edge is composed of a string representing its name, two string
   representing the names of the nodes it is connecting, a label that the
   user can define and a list of properties.

4. Each property is composed of two strings representing the name of the property
   and its type (respectively), and a DataType, which I will explain later.

5. A data type represents the basic types in Haskell, including the Undefined in 
   which I will use the symbol ⊥. Each data type will be represented by a letter:
    → I : Integer
    → D : Double
    → S : String
    → B : Bool
    → T : Date

6. A date will be represented by a string and not 3 numbers, to simplify the 
   input reading.    

7. A label will be represented by a string, representing its name.

8. If a function is meant to be called by the user (the query ones), then
   when a node or an edge is asked as a parameter, a string representing its 
   name will be asked instead to ease its use.

9. Although the input may be correct, I never assume it is, and given the case
   the user writes incorrect input, my code will return either the same graph
   or an error message, depending on the function.

10. Every property name will be converted to lower case to avoid cases in which
    by mistake the user writes a property that exists but uppercase. 
    i.e : the property birthday exists, but the user writes birthDay, thanks to 
    this section, the program will consider the same, because it turns the property
    given by the user to lowercase (turning it into "birthday")

11. Every function will be clearly documented before its code.

12. When reading input from the user, only strings will be asked unless anything 
    specific is necessary (such as properties of an edge, numbers, etc). 
    Besides, users won't have to introduce anything related to the graph 
    when being asked for input

13. When being asked for a function (in the case of kHops), the user will only have
    limited options: ==, /=, >=, >, <=, <. That is because from a console it is very
    difficult to define a function, and therefore it would make the process harder
    for the user. If the function inserted is none of them, then the default function
    will be \x y -> x /= S "".

14. Although the functions defVprop and defEprop have the functionalities to add
    multiple properties, the user only has the possibility to add one at a time.

###########################################################################
-}




-- ########################################################################
--                            DATA DECLARATION                             
-- ########################################################################
data Graph    = Graph [Node] [Edge] [Property]
data Node     = Node String Label [Property]
data Edge     = Edge String String String Label [Property]
data Property = Property String String DataType
data DataType = I Int | D Double | S String | B Bool | T Date | U String
    deriving (Eq, Ord) -- to be able to compare them
type Date     = String
type Label    = String


-- ########################################################################
--                            SHOW INSTANCES                                
-- ########################################################################
-- Show instance for Graph
instance Show Graph where
    show (Graph [] [] _)            = "\n"
    show (Graph [] (e:edges) ps)    = show e ++ "\n" ++ show (Graph [] edges ps)
    show (Graph (n:nodes) edges ps) = show n ++ "\n" ++ show (Graph nodes edges ps)

-- Show instance for Node
instance Show Node where
    show (Node "" ""  [])          = ""
    show (Node "" "" [p])          = show p
    show (Node "" "" (p:ps))       = show p ++ "," ++ show (Node "" "" ps)
    show (Node name label props)   = name ++ "[" ++ label ++ "]" ++ "{" ++ show (Node "" "" props) ++ "}"

-- Show instance for Edge
instance Show Edge where
    show (Edge "" "" "" "" [])     = ""
    show (Edge "" "" "" "" [p])    = show p
    show (Edge "" "" "" "" (p:ps)) = show p ++ "," ++ show (Edge "" "" "" "" ps)
    show (Edge name node1 node2 label props) =
        "(" ++ node1 ++ ")" ++ "−" ++ name ++ "[" ++ label ++ "]" ++ "->" ++
        "(" ++ node2 ++ ")" ++ "{" ++ show (Edge "" "" "" "" props) ++ "}"

-- Show instance for Property
instance Show Property where
    show (Property name _ dataType) = "(" ++ name ++ ", " ++ show dataType ++ ")"

-- Show instance for DataType
instance Show DataType where
    show (I value) = show value
    show (D value) = show value
    show (S value) = value
    show (B value) = show value
    show (T value) = value
    show (U value) = value

instance Eq Node where
    Node n1 _ _ == Node n2 _ _
        | n1 == n2  = True
        | otherwise = False

instance Eq Edge where
    Edge e1 _ _ _ _ == Edge e2 _ _ _ _
        | e1 == e2  = True
        | otherwise = False

instance Eq Property where
    Property p1 _ _ == Property p2 _ _
        | p1 == p2  = True
        | otherwise = False


-- ########################################################################
--                              OUTPUT FUNCTIONS                        
-- ########################################################################
-- Auxiliary function to print nodes and edges from a graph
printList :: Show a => [a] -> IO ()
printList []     = return ()
printList (x:xs) = do
    print x
    printList xs


-- ########################################################################
--                             AUXILIARY FUNCTIONS                   
-- ########################################################################
-- Converts the string to lower case. This is done to avoid errors when reading
-- the input files which contain the properties, because there can be errors
lC :: String -> String
lC word = [toLower c | c <- word]

-- Returns the same list without the first line
nextLine :: [String] -> [String]
nextLine = tail

-- Returns the same list without the last line
prevLine :: [String] -> [String]
prevLine = init

-- Returns the first element of a line
firstElement :: [String] -> String
firstElement    = head

-- Returns the second element of a line
secondElement :: [String] -> String
secondElement l = head $ tail l

-- Returns the third element of a line
thirdElement :: [String] -> String
thirdElement    = last

-- Returns two elements of a line
getTwoElements   :: [String] -> (String, String)
getTwoElements   s = (firstElement s, secondElement s)

-- Returns three elements of a line
getThreeElements :: [String] -> (String, String, String)
getThreeElements s = (elem1, elem2, thirdElement s)
    where
        (elem1, elem2) = getTwoElements s

-- Replaces the element given in the list
replaceElem :: Eq t => t -> [t] -> [t]
replaceElem _ [] = []
replaceElem elem (x:xs)
    | elem == x       = elem:xs
    | otherwise       = x:replaceElem elem xs

-- Removes the duplicate elements of a list
removeDups :: (Eq a) => [a] -> [a]
removeDups (x:xs) = x : removeDups (filter (/= x) xs)
removeDups [] = []

-- Returns an empty list if the list contains the element, otherwise
-- it returns a list containing the element
elemRepetead :: Eq a => [a] -> a -> [a]
elemRepetead [] x = [x]
elemRepetead xs x
    | x `elem` xs = []
    | otherwise   = [x]

-- Returns the node with the name given as a first parameter if it is
-- contained in the list, otherwise Nothing is returned
findNode :: String -> [Node] -> Maybe Node
findNode _ [] = Nothing
findNode name (n:ns)
    | getNodeName n == name = Just n
    | otherwise             = findNode name ns

-- Returns the edge with the name given as a first parameter if it is
-- contained in the list, otherwise Nothing is returned
findEdge :: String -> [Edge] -> Maybe Edge
findEdge _ [] = Nothing
findEdge name (e:es)
    | getEdgeName e == name = Just e
    | otherwise             = findEdge name es

-- Returns the property with the name given as a first parameter if it is
-- contained in the list, otherwise Nothing is returned
findProperty :: String -> [Property] -> Maybe Property
findProperty _ [] = Nothing
findProperty name (p:ps)
    | getPropName p == name = Just p
    | otherwise             = findProperty name ps

-- Returns true if the node contains the property, otherwise returns false
nodeContainsProperty :: Node -> Property -> Bool
nodeContainsProperty (Node _ _ ps) p = p `elem` ps

-- Returns true if the edge contains the property, otherwise returns false
edgeContainsProperty :: Edge -> Property -> Bool
edgeContainsProperty (Edge _ _ _ _ ps) p = p `elem` ps

-- Returns true if the node has a label, otherwise returns false
nodeContainsLabel :: Label -> Node -> Bool
nodeContainsLabel label (Node _ l _)
    | l == label   = True
    | otherwise    = False

-- Returns true if the edge has a label, otherwise returns false
edgeContainsLabel :: Label -> Edge -> Bool
edgeContainsLabel label (Edge _ _ _ l _)
    | l == label   = True
    | otherwise    = False

-- Auxiliary function to allow the readFile function to be called from
-- the populate function
fileRead :: FilePath -> String
fileRead path =
    inlinePerformIO $ readFile path


-- ########################################################################
--                              GETTERS, SETTERS                     
-- ########################################################################
-- Returns the name of the node
getNodeName :: Node -> String
getNodeName (Node n _ _ )    = n

-- Returns the name of the edge
getEdgeName :: Edge -> String
getEdgeName (Edge e _ _ _ _) = e

-- Returns the label of the node
getNodeLabel :: Node -> Label
getNodeLabel (Node _ l _)     = l

-- Returns the label of the edge
getEdgeLabel :: Edge -> Label
getEdgeLabel (Edge _ _ _ l _) = l

-- Returns the properties of the node
getNodeProperties :: Node -> [Property]
getNodeProperties (Node _ _ ps)     = ps

-- Returns the properties of the edge
getEdgeProperties :: Edge -> [Property]
getEdgeProperties (Edge _ _ _ _ ps) = ps

-- Returns the name of the property
getPropName :: Property -> String
getPropName (Property n _ _)   = n

-- Returns the type of the property
getPropType :: Property -> String
getPropType (Property _ t _)   = t

-- Returns the dataType of the property
getPropData :: Property -> DataType
getPropData (Property _ _ d)   = d

-- Sets the label of the node to the label given as a parameter
setNodeLabel :: Label -> Node -> Node
setNodeLabel l (Node n _ props)       = Node n l props

-- Sets the label of the edge to the label given as a parameter
setEdgeLabel :: Label -> Edge -> Edge
setEdgeLabel l (Edge e n1 n2 _ props) = Edge e n1 n2 l props

-- Sets the properties of the node to the data given as a parameter
setPropData  :: String -> Property -> Property
setPropData  d (Property name dataType _)
    | dataType == "Int"     = Property name dataType (I (read d::Int))
    | dataType == "Double"  = Property name dataType (D (read d::Double))
    | dataType == "String"  = Property name dataType (S d)
    | dataType == "Bool"    = Property name dataType (B (read d::Bool))
    | dataType == "Date"    = Property name dataType (T d)
    | otherwise             = Property name dataType (U "⊥")


-- ########################################################################
--                          AUXILIARY FUNCTIONS                    
--                       TO MAKE THE CODE LEGIBLE                    
-- ########################################################################
-- Adds a property to the node
addNodeProperty :: Property -> Node -> Node
addNodeProperty p (Node n l []) = Node n l [p]
addNodeProperty p (Node n l ps) = Node n l (ps ++ [p])

-- Adds a property to the edge
addEdgeProperty :: Property -> Edge -> Edge
addEdgeProperty p (Edge e n1 n2 l []) = Edge e n1 n2 l [p]
addEdgeProperty p (Edge e n1 n2 l ps) = Edge e n1 n2 l (ps ++ [p])

-- Auxiliary function that, by using defVLabel and defELabel, adds
-- a label to a node, edge or both
-- If the node/edge is not in the graph, the same graph is returned
includeLabels :: String -> Label -> Graph -> Graph
includeLabels name label (Graph ns es ps) = finalGraph
    where
        (node, edge) = (findNode name ns, findEdge name es)
        checkVlabel  = defVlabel (Graph ns es ps) (getNodeName $ fromJust node) label
        checkElabel  = defElabel auxGraph (getEdgeName $ fromJust edge) label
        auxGraph
            | isJust node && isLeft checkVlabel = fromLeft (Graph [] [] []) checkVlabel
            | otherwise   = Graph ns es ps
        finalGraph
            | isJust edge && isLeft checkElabel = fromLeft (Graph [] [] []) checkElabel
            | otherwise   = auxGraph

-- Auxiliary function that, by using defVprop and  defEprop, adds
-- a property to a node, edge or both
-- If the node/edge is not in the graph, the same graph is returned
includeProperties  :: String -> Property -> Graph -> Graph
includeProperties name prop (Graph ns es ps) = finalGraph
    where
        (node, edge) = (findNode name ns, findEdge name es)
        auxGraph
            | isJust node = defVprop (Graph ns es ps) (getNodeName $ fromJust node) [prop]
            | otherwise   = Graph ns es ps
        finalGraph
            | isJust edge = defEprop auxGraph (getEdgeName $ fromJust edge) [prop]
            | otherwise   = auxGraph

-- Auxiliary function that, given a name of a node, a label, a list of names
-- of nodes ("visited") and a list of edges, returns the names of nodes (that
-- don't appear in "visited") directly connected with the one given, only by
-- edges that contain the label given as a parameter
adjacentNodesWithSameLabel :: String -> Label -> [String] -> [Edge] -> [String]
adjacentNodesWithSameLabel _ _ _ [] = []
adjacentNodesWithSameLabel n label visited ((Edge _ n1 n2 l _):es)
    | n == n1 && notElem n2 visited && label == l = n2 : adjacentNodesWithSameLabel n label visited es
    | otherwise = adjacentNodesWithSameLabel n label visited es

-- Auxiliary function used for easing the legibility of sigma' in case
-- a node has been given as input
checkPropertiesNode :: Node -> [Property] -> [(String, DataType)]
checkPropertiesNode _ [] = []
checkPropertiesNode n (p:ps)
    | nodeContainsProperty n p = (getPropName p, getPropData propNode) : checkPropertiesNode n ps
    | otherwise = (getPropName p, U "⊥") : checkPropertiesNode n ps
    where propNode = fromJust $ findProperty (getPropName p) $ getNodeProperties n

-- Auxiliary function used for easing the legibility of sigma' in case
-- an edge has been given as input
checkPropertiesEdge :: Edge -> [Property] -> [(String, DataType)]
checkPropertiesEdge _ [] = []
checkPropertiesEdge e (p:ps)
    | edgeContainsProperty e p = (getPropName p, getPropData propEdge) : checkPropertiesEdge e ps
    | otherwise = (getPropName p, U "⊥") : checkPropertiesEdge e ps
    where propEdge = fromJust $ findProperty (getPropName p) $ getEdgeProperties e

-- Auxiliary function that returns the adjacent nodes to the one given as a
-- parameter
adjacentNodes :: String -> [Edge] -> [String]
adjacentNodes _ [] = []
adjacentNodes n ((Edge _ n1 n2 _ _):es)
    | n == n1   = n2 : adjacentNodes n es
    | otherwise = adjacentNodes n es

-- Auxiliary function that, given a list of nodes returns the list of all the
-- nodes adjacents to the ones in the list. Function used in kHops.
findAllAdjacents :: [String] -> [Edge] -> [String]
findAllAdjacents [] _ = []
findAllAdjacents (n:ns) es = adjacentNodes n es ++ findAllAdjacents ns es

-- The function returns the nodes that the nodes in the list can reach within k steps
kPath :: Integer -> [Edge] -> [String] -> [String]
kPath 0 _  nodes = nodes
kPath k es nodes = kPath (k-1) es $ findAllAdjacents nodes es


-- ########################################################################
--                              INTERPRETING FILES                       
-- ########################################################################
-- Interprets the contents of propFile where each line is composed of 2
-- elements, the first one represents the name of the property and the
-- second one represents the type of the property
interpretPropFile :: [String] -> [Property]
interpretPropFile []   = []
interpretPropFile line = Property (lC name) value (U "⊥") : interpretPropFile (nextLine line)
    where
        (name, value)  = getTwoElements lineWords
        lineWords      = words $ head line

-- Interprets the contents of rhoFile where each line is composed of 3
-- elements, the first one represents the name of the edge and the
-- other two elements represent names of nodes
interpretRhoFile :: Graph -> [String] -> Graph
interpretRhoFile graph []    = graph
interpretRhoFile (Graph ns es ps) line = interpretRhoFile finalGraph (nextLine line)
    where
        (e, n1, n2)          = getThreeElements lineWords
        (newNode1, newNode2) = (Node n1 "" [], Node n2 "" [])
        (newEdge, newNodes)  = (Edge e n1 n2 "" [], ns ++ elemRepetead ns newNode1 ++ elemRepetead ns newNode2)
        finalGraph           = addEdge (Graph newNodes es ps) newEdge (Node n1 "" []) (Node n2 "" [])
        lineWords            = words $ head line

-- Interprets the contents of lambdaFile where each line is composed of 2
-- elements, the first one represents the name of the node and the
-- second one represents the label of the node
interpretLambdaFile :: Graph -> [String] -> Graph
interpretLambdaFile graph []   = graph
interpretLambdaFile (Graph ns es ps) line = interpretLambdaFile finalGraph (nextLine line)
    where
        (name, label)  = getTwoElements lineWords
        finalGraph     = includeLabels name label $ Graph ns es ps
        lineWords      = words $ head line

-- Interprets the contents of sigmaFile where each line is composed of 3
-- elements, the first one represents the name of the node and the
-- second one represents the name of the property, and the third 
-- element represents the value of the property
interpretSigmaFile :: Graph -> [String] -> Graph
interpretSigmaFile graph [] = graph
interpretSigmaFile (Graph ns es ps) line = interpretSigmaFile finalGraph (nextLine line)
    where
        (name, p, val) = getThreeElements lineWords
        updatedProp    = setPropData val $ fromJust $ findProperty (lC p) ps
        finalGraph     = includeProperties name updatedProp $ Graph ns es ps
        lineWords      = words $ head line


-- ########################################################################
--                          PROPERTY GRAPHS IN HASKELL
-- ########################################################################
-- populate: String × String × String × String → PG
-- Populates a graph with the given files
populate :: String -> String -> String -> String -> Graph
populate propFile rhoFile lambdaFile sigmaFile = finalGraph
    where
        props          = interpretPropFile $ lines $ fileRead propFile
        graph1         = interpretRhoFile (Graph [] [] props) $ lines $ fileRead rhoFile
        graph2         = interpretLambdaFile graph1 $ lines $ fileRead lambdaFile
        finalGraph     = interpretSigmaFile graph2 $ lines $ fileRead sigmaFile

-- addEdge: PG × E × V × V → PG
-- Given two nodes, an edge and a graph, adds an edge connecting the two nodes 
-- to the given graph and returns the graph with the new edge included
-- If the edge already exists or the nodes doesn't exist it returns the same graph
addEdge :: Graph -> Edge -> Node -> Node -> Graph
addEdge (Graph nodes edges ps) (Edge e _ _ l p) (Node n1 _ _) (Node n2 _ _) = Graph nodes newEdges ps
    where
        (edge, node1, node2) = (findEdge e edges, findNode n1 nodes, findNode n2 nodes)
        newEdges
            | isJust edge || isNothing node1 || isNothing node2 = edges
            | otherwise   = edges ++ [Edge e n1 n2 l p]

-- Auxiliary function for defVprop
defVprop' :: String -> [Property] -> [Node] -> Property -> [Node]
defVprop' n ps nodes (Property p t dt) = newNodes
    where
        prop = findProperty (lC p) ps
        node = findNode n nodes
        newNodes
            | isNothing node || isNothing prop || nodeContainsProperty (fromJust node) (Property (lC p) t dt) = nodes
            | otherwise = replaceElem (addNodeProperty (Property (lC p) t dt) (fromJust node)) nodes

-- defVprop: PG × V × P(Prop×Val) → PG
-- Given a graph, a node name and a property list, adds the properties to the node
-- and returns the graph with the updated node
-- If the node is not in the graph, the property is already defined, the
-- property doesn't exist or the list of properties is empty the same graph is returned
defVprop :: Graph -> String -> [Property] -> Graph
defVprop (Graph nodes edges ps) n [] = Graph nodes edges ps
defVprop (Graph nodes edges ps) n ((Property p t dt):props) = defVprop (Graph newNodes edges ps) n props
    where
        newNodes = defVprop' n ps nodes prop
        prop = Property p t dt

-- Auxiliary function for defEprop
defEprop' :: String -> [Property] -> [Edge] -> Property -> [Edge]
defEprop' e ps edges (Property p t dt)= newEdges
    where
        prop = findProperty (lC p) ps
        edge = findEdge e edges
        newEdges
            | isNothing edge || isNothing prop || edgeContainsProperty (fromJust edge) (Property (lC p) t dt) = edges
            | otherwise = replaceElem (addEdgeProperty (Property (lC p) t dt) (fromJust edge)) edges

-- defEprop: PG × E × P(Prop×Val) → PG
-- Given a graph, an edge name and a property list, adds the properties to the node
-- and returns the graph with the updated edge
-- If the edge is not in the graph or the property is already defined, or the
-- property doesn't exist or the list of properties is empty the same graph is returned
defEprop :: Graph -> String -> [Property] -> Graph
defEprop (Graph nodes edges ps) e [] = Graph nodes edges ps
defEprop (Graph nodes edges ps) e (Property p t dt:props) = defEprop (Graph nodes newEdges ps) e props
    where
        newEdges = defEprop' e ps edges prop
        prop = Property p t dt

-- defVlabel: PG × V × Lab → PG ∪ Error
-- Given a graph, a node name and a label, if the node already contains a label,
-- an error message is returned, otherwise the label is included to the node
-- and the graph with the updated node is returned
-- If the node is not in the graph, the same graph is returned
defVlabel :: Graph -> String -> Label -> Either Graph String
defVlabel (Graph nodes edges ps) n l
    | isNothing node = Left (Graph nodes edges ps)
    | not $ nodeContainsLabel "" $ fromJust node = Right "ERROR: Label is already defined"
    | otherwise = Left (Graph newNodes edges ps)
    where
        node = findNode n nodes
        newNodes
            | isJust node = replaceElem (setNodeLabel l (fromJust node)) nodes
            | otherwise   = nodes

-- defElabel: PG × E × Lab → PG ∪ Error
-- Given a graph, an edge name and a label, if the edge already contains a label,
-- an error message is returned, otherwise the label is included to the edge
-- and the graph with the updated edge is returned
-- If the edge is not in the graph, the same graph is returned
defElabel :: Graph -> String -> Label -> Either Graph String
defElabel (Graph nodes edges ps) e l
    | isNothing edge = Left (Graph nodes edges ps)
    | not $ edgeContainsLabel "" $ fromJust edge = Right "ERROR: Label is already defined"
    | otherwise = Left (Graph nodes newEdges ps)
    where
        edge = findEdge e edges
        newEdges
            | isJust edge = replaceElem (setEdgeLabel l (fromJust edge)) edges
            | otherwise   = edges

-- showGraph: PG → (V,E,Lab,Prop,ρ,λ,σ)
-- Function to print graphs
showGraph :: Graph -> IO()
showGraph (Graph v e _)= do
    printList v
    printList e
    putStrLn ""


-- ########################################################################
--                    QUERYING AGAINST PROPERTY GRAPHS
-- ########################################################################
-- σ': PG × (V ∪ E) → P(Prop × Val)
-- Given a name that can represent a node or an edge, returns a tuple in which the 
-- first element is the name of the property and the second element is the value of
-- this property in the node/edge. If the node/edge doesn't contain this property,
-- then ⊥ is shown
sigma' :: Graph -> String -> [(String, DataType)]
sigma' (Graph ns es ps) name = props
    where
        (node, edge) = (findNode name ns, findEdge name es)
        props
            | isJust node = checkPropertiesNode (fromJust node) ps
            | isJust edge = checkPropertiesEdge (fromJust edge) ps
            | otherwise = []

-- propV: PG × Nat × Prop → P(V × Val)
-- Given a graph, a natural number "k" and a property name, returns a list containing the 
-- first "k" nodes that have the property given
propV :: Graph -> Integer -> String -> [(Label, DataType)]
propV  _                   0 _    = []
propV (Graph [] _ _)       k prop = []
propV (Graph (n:ns) es ps) k prop = returnValue
    where
        p = lC prop
        property = findProperty p (getNodeProperties n)
        returnValue
            | isJust property = (getNodeLabel n, getPropData (fromJust property)) : propV (Graph ns es ps) (k-1) p
            | otherwise = propV (Graph ns es ps) k p

-- propE: PG × Nat × Prop → P(E × Val)
-- Given a graph, a natural number "k" and a property name, returns a list containing the 
-- first "k" edges that have the property given
propE :: Graph -> Integer -> String -> [(Label, DataType)]
propE  _                   0 _    = []
propE (Graph _      [] _)  k prop = []
propE (Graph ns (e:es) ps) k prop = returnValue
    where
        p = lC prop
        property = findProperty p (getEdgeProperties e)
        returnValue
            | isJust property = (getEdgeLabel e, getPropData (fromJust property)) : propE (Graph ns es ps) (k-1) p
            | otherwise = propE (Graph ns es ps) k p

-- kHops: PG × Nat × V × Prop × (Val × Val → Bool) × Val → P(V × Lab × Val)
-- Given a natural number, a node name, a property name, a predicate and a data type, a list of triplets
-- in this format (node name, node label, data type) is returned such that every node in the list has
-- the property given, and this property's datatype fulfills the predicate given as a parameter.
kHops :: Graph -> Integer -> String -> String -> (DataType -> DataType -> Bool) -> DataType -> [(String, Label, DataType)]
kHops (Graph ns es ps) k name p f val = removeDups valid
    where
        node  = fromJust $ findNode name ns
        nodes = [fromJust $ findNode n ns | n <- kPath k es [name]]
        prop  = findProperty (lC p) $ getNodeProperties node
        props = [(getNodeName n, getNodeLabel n, getData n) | n <- nodes, nodeContainsProperty n $ fromJust prop]
        getData = getPropData . fromJust . findProperty (lC p) . getNodeProperties
        valid
            | isJust prop = filter (\(n,l,dt) -> f dt val) props
            | otherwise   = []

-- reachable: PG × V × V × Lab → Bool
-- Given a graph, two nodes names and a label, returns true if there is path from the first node given 
-- to the second node given where all the edges in the traversal have the label given, otherwise
-- returns false
reachable :: Graph -> String -> String -> Label -> Bool
reachable (Graph ns es _) start target l = reachable' es target l [start] [start]
    where
        reachable' _  _  _  _ [] = False
        reachable' es target l visited (n:ns)
            | n == target || elem n ns = True
            | otherwise = reachable' es target l newVisited $ ns ++ adjacentNodesWithSameLabel n l newVisited es
            where newVisited = n:visited


-- ########################################################################
--                              IN/OUT FUNCTIONS                                  
-- ########################################################################
-- Auxiliary function to make the code cleaner
fullTemplate :: String -> String
fullTemplate s = "Insert the name of the " ++ s ++  " file with the proper extension: "

-- Auxiliary function to make the code cleaner
jumpLine :: IO ()
jumpLine = putStrLn ""

-- Auxiliary function to show the options that the user has regarding the first
-- part of the project
optionsPart1 :: IO()
optionsPart1 = do
    putStrLn "Property graphs possible commands: "
    putStrLn "   addEdge: Edge Node Node → Graph"
    putStrLn "   defVprop: Node Property → Graph"
    putStrLn "   defEprop: Edge Property → Graph"
    putStrLn "   defVlabel: Node Label → Graph"
    putStrLn "   defElabel: Node Label → Graph"
    putStrLn "   showGraph: → Graph"

-- Auxiliary function to show the options that the user has regarding the second
-- part of the project
optionsPart2 :: IO()
optionsPart2 = do
    putStrLn "Querying against property graphs possible commands: "
    putStrLn "   sigma': Node or Edge → [String, DataType]"
    putStrLn "   propV: Natural Property → [Node, DataType]"
    putStrLn "   propE: Natural Property → [Edge, DataType]"
    putStrLn "   kHops: Natural Node Property Function DataType → [Node, Label, DataType]"
    putStrLn "   reachable: Node Node Label → Bool"

-- Auxiliary function to guide the user through the process of the function "addEdge"
showOptsAddEdge :: Graph -> IO()
showOptsAddEdge graph = do
    putStrLn "Insert an edge name: "
    e <- getLine
    jumpLine
    putStrLn "Insert a label for the edge: "
    l <- getLine
    jumpLine
    putStrLn "Insert a node name: "
    n1 <- getLine
    jumpLine
    putStrLn "Insert a node name: "
    n2 <- getLine
    jumpLine
    let propGraph = addEdge graph (Edge e n1 n2 l []) (Node n1 "" []) (Node n2 "" [])
    showGraph propGraph

-- Auxiliary function to translate a string to a boolean data type
showOptsBool :: String -> DataType
showOptsBool s
    | lC s == "true" || lC s == "1" = B True
    | otherwise = B False

-- Auxiliary function to guide the user through the process of the function "defVprop"
showOptsDefVprop :: Graph -> IO ()
showOptsDefVprop graph = do
    putStrLn "Insert a node name: "
    n <- getLine
    jumpLine
    putStrLn "Insert a property name: "
    p <- getLine
    jumpLine
    putStrLn "Insert the type of the property (int, double, string, bool, date): "
    dt <- getLine
    jumpLine
    putStrLn "Insert the value of the property: "
    val <- getLine
    jumpLine
    when (lC dt == "int") $ do
        let propGraph = defVprop graph n [Property p dt $ I (read val :: Int)]
        showGraph propGraph
    when (lC dt == "string") $ do
        let propGraph = defVprop graph n [Property p dt $ S val]
        showGraph propGraph
    when (lC dt == "bool") $ do
        let propGraph = defVprop graph n [Property p dt (showOptsBool val)]
        showGraph propGraph
    when (lC dt == "double") $ do
        let propGraph = defVprop graph n [Property p dt $ D (read val :: Double)]
        showGraph propGraph
    when (lC dt == "date") $ do
        let propGraph = defVprop graph n [Property p dt (T val)]
        showGraph propGraph

-- Auxiliary function to guide the user through the process of the function "defEprop"
showOptsDefEprop :: Graph -> IO ()
showOptsDefEprop graph = do
    putStrLn "Insert an edge name: "
    e <- getLine
    jumpLine
    putStrLn "Insert a property name: "
    p <- getLine
    jumpLine
    putStrLn "Insert the type of the property (int, double, string, bool, date): "
    dt <- getLine
    jumpLine
    putStrLn "Insert the value of the property: "
    val <- getLine
    jumpLine
    when (lC dt == "int") $ do
        let propGraph = defEprop graph e [Property p dt $ I (read val :: Int)]
        showGraph propGraph
    when (lC dt == "string") $ do
        let propGraph = defEprop graph e [Property p dt $ S val]
        showGraph propGraph
    when (lC dt == "bool") $ do
        let propGraph = defEprop graph e [Property p dt (showOptsBool val)]
        showGraph propGraph
    when (lC dt == "double") $ do
        let propGraph = defEprop graph e [Property p dt $ D (read val :: Double)]
        showGraph propGraph
    when (lC dt == "date") $ do
        let propGraph = defEprop graph e [Property p dt $ T val]
        showGraph propGraph

-- Auxiliary function to guide the user through the process of the function "defVlabel"
showOptsDefVlabel :: Graph -> IO ()
showOptsDefVlabel graph = do
    putStrLn "Insert an edge name: "
    n <- getLine
    jumpLine
    putStrLn "Insert the label: "
    l <- getLine
    jumpLine
    let propGraph = defVlabel graph n l
    if isLeft propGraph then showGraph $ fromLeft (Graph [] [] []) propGraph
    else print $ fromRight [] propGraph

-- Auxiliary function to guide the user through the process of the function "defElabel"
showOptsDefElabel :: Graph -> IO ()
showOptsDefElabel graph = do
    putStrLn "Insert an edge name: "
    e <- getLine
    jumpLine
    putStrLn "Insert the label: "
    l <- getLine
    jumpLine
    let propGraph = defElabel graph e l
    if isLeft propGraph then showGraph $ fromLeft (Graph [] [] []) propGraph
    else print $ fromRight [] propGraph

-- Auxiliary function to guide the user through the process of the function "sigma'"
showOptsSigma :: Graph -> IO ()
showOptsSigma graph = do
    putStrLn "Insert a node or edge name: "
    name <- getLine
    jumpLine
    print $ sigma' graph name

-- Auxiliary function to guide the user through the process of the function "propV"
showOptsPropV :: Graph -> IO ()
showOptsPropV graph = do
    putStrLn "Insert a number: "
    k <- getLine
    let kNUM = read k::Integer
    jumpLine
    putStrLn "Insert a property name: "
    p <- getLine
    jumpLine
    print $ propV graph kNUM p

-- Auxiliary function to guide the user through the process of the function "propE"
showOptsPropE :: Graph -> IO ()
showOptsPropE graph = do
    putStrLn "Insert a number: "
    k <- getLine
    let kNUM = read k::Integer
    jumpLine
    putStrLn "Insert a property name: "
    p <- getLine
    jumpLine
    print $ propE graph kNUM p

-- Auxiliary function that returns a function depending on the user's input.
showOptsKHops' :: String -> (DataType -> DataType -> Bool)
showOptsKHops' s
  | s == "==" = (==)
  | s == "/=" = (/=)
  | s == ">=" = (>=)
  | s == ">"  = (>)
  | s == "<=" = (<=)
  | s == "<"  = (<)
  | otherwise = \x y -> x /= S ""

-- Auxiliary function to guide the user through the process of the function "kHops"
showOptskHops :: Graph -> IO ()
showOptskHops graph = do
    putStrLn "Insert a number: "
    k <- getLine
    let kNUM = read k::Integer
    jumpLine
    putStrLn "Insert a node name: "
    n <- getLine
    jumpLine
    putStrLn "Insert a property name: "
    p <- getLine
    jumpLine
    putStrLn "Insert a property type (int, double, string, bool, date): "
    dt <- getLine
    jumpLine
    putStrLn "Insert a property value: "
    val <- getLine
    jumpLine
    putStrLn "Insert a function (==, /=, >=, >, <=, <): "
    func <- getLine
    jumpLine
    when (lC dt == "int") $
        print $ kHops graph kNUM n p (showOptsKHops' func) $ I (read val :: Int)
    when (lC dt == "string") $
        print $ kHops graph kNUM n p (showOptsKHops' func) (S val)
    when (lC dt == "bool") $
        print $ kHops graph kNUM n p (showOptsKHops' func) $ showOptsBool val
    when (lC dt == "double") $
        print $ kHops graph kNUM n p (showOptsKHops' func) $ D (read val :: Double)
    when (lC dt == "date") $
        print $ kHops graph kNUM n p (showOptsKHops' func) $ T val

-- Auxiliary function to guide the user through the process of the function "reachable"
showOptsReachable :: Graph -> IO ()
showOptsReachable graph = do
    putStrLn "Insert a node name: "
    n1 <- getLine
    jumpLine
    putStrLn "Insert a node name: "
    n2 <- getLine
    jumpLine
    putStrLn "Insert the label: "
    l <- getLine
    jumpLine
    print $ reachable graph n1 n2 l


-- ########################################################################
--                                   MAIN                                  
-- ########################################################################
main :: IO ()
main = do
    jumpLine
    putStrLn "######################## LLENGUATGES DE PROGRAMACIÓ ########################"
    putStrLn "                                                        Author: Pau Vallespí"
    jumpLine
    optionsPart1
    jumpLine
    optionsPart2
    jumpLine

    putStrLn $ fullTemplate "prop"
    propFile   <- getLine
    jumpLine

    putStrLn $ fullTemplate "rho"
    rhoFile    <- getLine
    jumpLine

    putStrLn $ fullTemplate "lambda"
    lambdaFile <- getLine
    jumpLine

    putStrLn $ fullTemplate "sigma"
    sigmaFile  <- getLine
    jumpLine
    putStrLn "############################################################################"
    jumpLine

    let propGraph = populate propFile rhoFile lambdaFile sigmaFile

    putStrLn "Graph populated! Insert one of the commands previously mentioned: "
    command <- getLine
    jumpLine

    -- Part 1
    when (command == "addEdge") (showOptsAddEdge propGraph)
    when (command == "defVprop") (showOptsDefVprop propGraph)
    when (command == "defEprop") (showOptsDefEprop propGraph)
    when (command == "defVlabel") (showOptsDefVlabel propGraph)
    when (command == "defElabel") (showOptsDefElabel propGraph)
    when (command == "showGraph") (showGraph propGraph)

    -- Part 2
    when (command == "sigma'") (showOptsSigma propGraph)
    when (command == "propV") (showOptsPropV propGraph)
    when (command == "propE") (showOptsPropE propGraph)
    when (command == "kHops") (showOptskHops propGraph)
    when (command == "reachable") (showOptsReachable propGraph)

    jumpLine
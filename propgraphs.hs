import System.IO
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe

{-
######################### LLENGUATGES DE PROGRAMACIÓ #########################
                            · Grup:         11                               
                            · Alumne:       Pau Vallespí                       
                            · Professor:    Jordi Petit                       
##############################################################################


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
-}

-- ########################################################################
--                            DATA DECLARATION                             
-- ########################################################################
data Graph    = Graph [Node] [Edge]
data Node     = Node String Label [Property]
data Edge     = Edge String String String Label [Property]
data Property = Property String String DataType
data DataType = I Int | D Double | S String | B Bool | T Date | U Char
type Date     = String
type Label    = String


-- ########################################################################
--                            SHOW INSTANCES                                
-- ########################################################################
instance Show Graph where
    show (Graph [] [])             = "\n"
    show (Graph [] (e:edges))      = show e ++ "\n" ++ show (Graph [] edges)
    show (Graph (n:nodes) edges)   = show n ++ "\n" ++ show (Graph nodes edges)

instance Show Node where
    show (Node "" ""  [])          = ""
    show (Node "" "" [p])          = show p
    show (Node "" "" (p:ps))       = show p ++ "," ++ show (Node "" "" ps)
    show (Node name label props)   = name ++ "[" ++ label ++ "]" ++ "{" ++ show (Node "" "" props) ++ "}"

instance Show Edge where
    show (Edge "" "" "" "" [])     = ""
    show (Edge "" "" "" "" [p])    = show p
    show (Edge "" "" "" "" (p:ps)) = show p ++ "," ++ show (Edge "" "" "" "" ps)
    show (Edge name node1 node2 label props) =
        "(" ++ node1 ++ ")" ++ "−" ++ name ++ "[" ++ label ++ "]" ++ "->" ++
        "(" ++ node2 ++ ")" ++ "{" ++ show (Edge "" "" "" "" props) ++ "}"

instance Show Property where
    show (Property name _ dataType) = "(" ++ name ++ ", " ++ show dataType ++ ")"

instance Show DataType where
    show (I value) = show value
    show (D value) = show value
    show (S value) = value
    show (B value) = show value
    show (T value) = value
    show (U value) = show value

instance Eq Node where
    Node n1 _ _ == Node n2 _ _
        | n1 == n2  = True
        | otherwise = False

instance Eq Edge where
    Edge e1 _ _ _ _ == Edge e2 _ _ _ _
        | e1 == e2  = True
        | otherwise = False


-- ########################################################################
--                              OUTPUT FUNCTIONS                        
-- ########################################################################
-- Auxiliary function to print nodes and edges from a graph
printList :: Show a => [a] -> IO ()
printList []     = do return ()
printList (x:xs) = do
    print x
    printList xs


-- ########################################################################
--                             AUXILIARY FUNCTIONS                   
-- ########################################################################
-- Returns the next line of a list of strings
nextLine :: [String] -> [String]
nextLine = tail

-- Returns the previous line of a list of strings
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

-- Removes the given element from the given list
removeElem :: Eq a => a -> [a] -> [a]
removeElem _ [] = []
removeElem x (y:ys)
    | x == y    = removeElem x ys
    | otherwise = y : removeElem x ys

-- Replaces the element given in the list
replaceElem :: Eq t => t -> [t] -> [t]
replaceElem _ [] = []
replaceElem elem (x:xs)
    | elem == x       = elem:xs
    | otherwise       = x:replaceElem elem xs

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


-- ########################################################################
--                              GETTERS, SETTERS                     
-- ########################################################################
-- Returns the name of the node
getNodeName :: Node -> String
getNodeName (Node n _ _ )    = n

-- Returns the name of the edge
getEdgeName :: Edge -> String
getEdgeName (Edge e _ _ _ _) = e

-- Returns the name of the property
getPropName :: Property -> String
getPropName (Property n _ _)   = n

-- Returns the type of the property
getPropType :: Property -> String
getPropType (Property _ t _)   = t

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
    | otherwise             = Property name dataType (U '⊥')


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

-- Function to include the labels to the nodes and edges
includeLabels :: String -> Maybe Node -> [Node] -> Maybe Edge -> [Edge] -> ([Node], [Edge])
includeLabels label node ns edge es = (nodes, edges)
    where
        nodes
            | isJust node = replaceElem (setNodeLabel label $ fromJust node) ns
            | otherwise   = ns
        edges
            | isJust edge = replaceElem (setEdgeLabel label $ fromJust edge) es
            | otherwise   = es

-- Function to include the properties to the nodes and edges
includeProperties  :: Property -> Maybe Node -> [Node] -> Maybe Edge -> [Edge] -> ([Node], [Edge])
includeProperties prop node ns edge es = (nodes, edges)
    where
        nodes
            | isJust node = replaceElem (addNodeProperty prop (fromJust node)) ns
            | otherwise   = ns
        edges
            | isJust edge = replaceElem (addEdgeProperty prop (fromJust edge)) es
            | otherwise   = es


-- ########################################################################
--                              INTERPRETING FILES                       
-- ########################################################################
-- Interprets the contents of propFile where each line is composed of 2
-- elements, the first one represents the name of the property and the
-- second one represents the type of the property
interpretPropFile :: [String] -> [Property]
interpretPropFile []   = []
interpretPropFile line = Property name value (U '⊥') : interpretPropFile (nextLine line)
    where
        (name, value)  = getTwoElements lineWords
        lineWords      = words $ head line

-- Interprets the contents of rhoFile where each line is composed of 3
-- elements, the first one represents the name of the edge and the
-- other two elements represent names of nodes
interpretRhoFile :: Graph -> [String] -> Graph
interpretRhoFile graph []    = graph
interpretRhoFile (Graph ns es) line  = interpretRhoFile graph (nextLine line)
    where
        (e, n1, n2)          = getThreeElements lineWords
        (newNode1, newNode2) = (Node n1 "" [], Node n2 "" [])
        (newEdge, newNodes)  = (Edge e n1 n2 "" [], ns ++ elemRepetead ns newNode1 ++ elemRepetead ns newNode2)
        graph                = addEdge (Graph newNodes es) newEdge (Node n1 "" []) (Node n2 "" [])
        lineWords            = words $ head line

-- Interprets the contents of lambdaFile where each line is composed of 2
-- elements, the first one represents the name of the node and the
-- second one represents the label of the node
interpretLambdaFile :: Graph -> [String] -> Graph
interpretLambdaFile graph []   = graph
interpretLambdaFile (Graph ns es) line = interpretLambdaFile (Graph nodes edges) (nextLine line)
    where
        (name, label)  = getTwoElements lineWords
        (node, edge)   = (findNode name ns, findEdge name es)
        (nodes, edges) = includeLabels label node ns edge es
        lineWords      = words $ head line

-- Interprets the contents of sigmaFile where each line is composed of 3
-- elements, the first one represents the name of the node and the
-- second one represents the name of the property, and the third 
-- element represents the value of the property
interpretSigmaFile :: Graph -> [Property] -> [String] -> Graph
interpretSigmaFile graph _ []    = graph
interpretSigmaFile (Graph ns es) ps line = interpretSigmaFile (Graph nodes edges) ps (nextLine line)
    where
        (name, p, val) = getThreeElements lineWords
        (node, edge)   = (findNode name ns, findEdge name es)
        (nodes, edges) = includeProperties updatedProp node ns edge es
        updatedProp    = setPropData val $ fromJust $ findProperty p ps
        lineWords      = words $ head line

-- ########################################################################
--                          PROPERTY GRAPHS IN HASKELL
-- ########################################################################
-- Populates a graph with the given files
populate :: String -> String -> String -> String -> Graph
populate propFile rhoFile lambdaFile sigmaFile = finalGraph
    where
        emptyGraph     = Graph [] []
        props          = interpretPropFile (lines propFile)
        graph1         = interpretRhoFile emptyGraph (lines rhoFile)
        graph2         = interpretLambdaFile graph1 (lines lambdaFile)
        finalGraph     = interpretSigmaFile graph2 props (lines sigmaFile)

-- Adds an edge to the graph
-- addEdge: PG × E × V × V → PG
-- FALTA COMPROVAR QUE NO HI SIGUI
addEdge :: Graph -> Edge -> Node -> Node -> Graph
addEdge (Graph nodes edges) (Edge e _ _ l p) (Node n1 _ _) (Node n2 _ _) = Graph nodes (edges ++ [newEdge])
    where
        newEdge = Edge e n1 n2 l p

-- defVprop: PG × V × P(Prop×Val) → PG


-- defEprop: PG × E × P(Prop×Val) → PG


-- defVlabel: PG × V × Lab → PG ∪ Error


-- defElabel: PG × E × Lab → PG ∪ Error


-- Function to print graphs
-- showGraph: PG → (V,E,Lab,Prop,ρ,λ,σ)
showGraph :: Graph -> IO()
showGraph (Graph v e)= do
    printList v
    printList e
    putStrLn ""


-- ########################################################################
--                                   MAIN                                  
-- ########################################################################
main = do
    {-
    propFileName   <- getLine 
    rhoFileName    <- getLine
    lambdaFileName <- getLine
    sigmaFileName  <- getLine -}

    propFile       <- readFile "propFile.pg" --readFile propFileName
    rhoFile        <- readFile "rhoFile.pg" --readFile rhoFileName
    lambdaFile     <- readFile "lambdaFile.pg" --readFile lambdaFileName
    sigmaFile      <- readFile "sigmaFile.pg" --readFile sigmaFileName

    let propGraph = populate propFile rhoFile lambdaFile sigmaFile
    --let graph2 = addEdge propGraph (Edge "eee" "p" "a" "paulex" []) (Node "p" "" []) (Node "a" "" [])
    print propGraph

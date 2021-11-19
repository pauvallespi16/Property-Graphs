import System.IO
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Either
import GHC.Natural (Natural)

{-
######################### LLENGUATGES DE PROGRAMACIÓ #########################
                            · Grup:      11                               
                            · Alumne:    Pau Vallespí                       
                            · Professor: Jordi Petit                       
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

instance Eq Property where
    Property p1 _ _ == Property p2 _ _
        | p1 == p2  = True
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

-- Auxiliary function that, by using defVLabel and defELabel, adds
-- a label to a node, edge or both
includeLabels :: String -> Label -> [Node] -> [Edge] -> Graph
includeLabels name label ns es = finalGraph
    where
        node = findNode name ns
        edge = findEdge name es
        auxGraph
            | isJust node = fromLeft (Graph [] []) (defVlabel (Graph ns es) (fromJust node) label)
            | otherwise   = Graph ns es
        finalGraph
            | isJust edge = fromLeft (Graph [] []) (defElabel auxGraph (fromJust edge) label)
            | otherwise   = auxGraph

-- Function to include the properties to the nodes and edges
includeProperties  :: String -> Property -> [Node] -> [Edge] -> Graph
includeProperties name prop ns es = finalGraph
    where
        node = findNode name ns
        edge = findEdge name es
        auxGraph
            | isJust node = defVprop (Graph ns es) (fromJust node) prop
            | otherwise   = Graph ns es
        finalGraph
            | isJust edge = defEprop auxGraph (fromJust edge) prop
            | otherwise   = auxGraph


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
interpretRhoFile (Graph ns es) line = interpretRhoFile finalGraph (nextLine line)
    where
        (e, n1, n2)          = getThreeElements lineWords
        (newNode1, newNode2) = (Node n1 "" [], Node n2 "" [])
        (newEdge, newNodes)  = (Edge e n1 n2 "" [], ns ++ elemRepetead ns newNode1 ++ elemRepetead ns newNode2)
        finalGraph           = addEdge (Graph newNodes es) newEdge (Node n1 "" []) (Node n2 "" [])
        lineWords            = words $ head line

-- Interprets the contents of lambdaFile where each line is composed of 2
-- elements, the first one represents the name of the node and the
-- second one represents the label of the node
interpretLambdaFile :: Graph -> [String] -> Graph
interpretLambdaFile graph []   = graph
interpretLambdaFile (Graph ns es) line = interpretLambdaFile finalGraph (nextLine line)
    where
        (name, label)  = getTwoElements lineWords
        finalGraph     = includeLabels name label ns es
        lineWords      = words $ head line

-- Interprets the contents of sigmaFile where each line is composed of 3
-- elements, the first one represents the name of the node and the
-- second one represents the name of the property, and the third 
-- element represents the value of the property
interpretSigmaFile :: Graph -> [Property] -> [String] -> Graph
interpretSigmaFile graph _ []    = graph
interpretSigmaFile (Graph ns es) ps line = interpretSigmaFile finalGraph ps (nextLine line)
    where
        (name, p, val) = getThreeElements lineWords
        updatedProp    = setPropData val $ fromJust $ findProperty p ps
        finalGraph     = includeProperties name updatedProp ns es
        lineWords      = words $ head line

-- ########################################################################
--                          PROPERTY GRAPHS IN HASKELL
-- ########################################################################
-- Populates a graph with the given files
populate :: String -> String -> String -> String -> Graph
populate propFile rhoFile lambdaFile sigmaFile = finalGraph
    where
        props          = interpretPropFile (lines propFile)
        graph1         = interpretRhoFile (Graph [] []) (lines rhoFile)
        graph2         = interpretLambdaFile graph1 (lines lambdaFile)
        finalGraph     = interpretSigmaFile graph2 props (lines sigmaFile)

-- Adds an edge to the graph
-- addEdge: PG × E × V × V → PG findEdge :: String -> [Edge] -> Maybe Edge
addEdge :: Graph -> Edge -> Node -> Node -> Graph
addEdge (Graph nodes edges) (Edge e _ _ l p) (Node n1 _ _) (Node n2 _ _) = Graph nodes newEdges
    where
        edge = findEdge e edges
        newEdges
            | isJust edge = edges
            | otherwise   = edges ++ [Edge e n1 n2 l p]

-- defVprop: PG × V × P(Prop×Val) → PG
defVprop :: Graph -> Node -> Property -> Graph
defVprop (Graph nodes edges) n p = Graph newNodes edges
    where
        node = findNode (getNodeName n) nodes
        newNodes
            | nodeContainsProperty n p = nodes
            | otherwise = replaceElem (addNodeProperty p (fromJust node)) nodes

-- defEprop: PG × E × P(Prop×Val) → PG
defEprop :: Graph -> Edge -> Property -> Graph
defEprop (Graph nodes edges) e p = Graph nodes newEdges
    where
        edge = findEdge (getEdgeName e) edges
        newEdges
            | edgeContainsProperty e p = edges
            | otherwise = replaceElem (addEdgeProperty p (fromJust edge)) edges

-- defVlabel: PG × V × Lab → PG ∪ Error
defVlabel :: Graph -> Node -> Label -> Either Graph String
defVlabel (Graph nodes edges) n l
    | not $ nodeContainsLabel "" $ fromJust node = Right "ERROR: Label is already defined"
    | otherwise = Left (Graph newNodes edges)
    where
        node     = findNode (getNodeName n) nodes
        newNodes = replaceElem (setNodeLabel l (fromJust node)) nodes

-- defElabel: PG × E × Lab → PG ∪ Error
defElabel :: Graph -> Edge -> Label -> Either Graph String
defElabel (Graph nodes edges) e l
    | not $ edgeContainsLabel "" $ fromJust edge = Right "ERROR: Label is already defined"
    | otherwise = Left (Graph nodes newEdges)
    where
        edge     = findEdge (getEdgeName e) edges
        newEdges = replaceElem (setEdgeLabel l (fromJust edge)) edges

-- Function to print graphs
-- showGraph: PG → (V,E,Lab,Prop,ρ,λ,σ)
showGraph :: Graph -> IO()
showGraph (Graph v e)= do
    printList v
    printList e
    putStrLn ""


-- ########################################################################
--                    QUERYING AGAINST PROPERTY GRAPHS
-- ########################################################################

-- σ: PG × (V ∪ E) → P(Prop × Val)

-- propV: PG × Nat × Prop → P(V × Val)
propV :: Graph -> Natural -> Property -> [(Label, DataType)]
propV  _                0 _ = []
propV (Graph [] _)      k p = []
propV (Graph (n:ns) es) k p = returnValue
    where
        property = findProperty (getPropName p) (getNodeProperties n)
        returnValue
            | isJust property = (getNodeLabel n, getPropData (fromJust property)) : propV (Graph ns es) (k-1) p
            | otherwise = propV (Graph ns es) k p


-- propE: PG × Nat × Prop → P(E × Val)
propE :: Graph -> Natural -> Property -> [(Label, DataType)]
propE  _                0 _ = []
propE (Graph _      []) k p = []
propE (Graph ns (e:es)) k p = returnValue
    where
        property = findProperty (getPropName p) (getEdgeProperties e)
        returnValue
            | isJust property = (getEdgeLabel e, getPropData (fromJust property)) : propV (Graph ns es) (k-1) p
            | otherwise = propE (Graph ns es) k p
            
-- kHops: PG × Nat × V × Prop × (Val × Val → Bool) × Val → P(V × Lab × Val)
-- reachable: PG × V × V × Lab → Bool

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
    {-let graph2 = defVprop propGraph (Node "i6" "Image" []) (Property "since" "String" (T "13-4-98"))
    let graph3 = defEprop graph2 (Edge "f4" "" "" "" []) (Property "since" "String" (T "13-4-1235"))
    let graph4 = defVlabel graph3 (Node "n1" "" []) "cipote"
    let graph5 = defElabel (fromLeft (Graph [] []) graph4) (Edge "ed4" "" "" "" []) "pechugo"-}

    let cositas = propV propGraph 5 (Property "gender" "String" (S ""))
    print cositas

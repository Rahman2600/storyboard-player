import SourceData 

-- Node Title Story Options
data Node = Node String String [Edge] | MtNode deriving (Show, Eq) 

-- Edge Option EndNode
data Edge = Edge String Node deriving (Show, Eq)

storyToNode:: Story -> Node
storyToNode (Story _ (StoryInfo []) scene_meta) = MtNode 
storyToNode (Story _ (StoryInfo (h:t)) scene_meta) = 
    (Node (scene_name h) (getStory (scene_id h) scene_meta)
        (map (\sp -> 
            (Edge (label sp) (storyToNode (Story "" (StoryInfo (getLinkedScenes (links sp) t)) 
        scene_meta)))) 
        (filter (\sp-> (not (port_in sp))) (scene_ports h))))

-- else (map (\ss -> (Edge (label (head (scene_ports ss))) (storyToNode (Story "" (StoryInfo [ss]) scene_meta)))) 
-- (getLinkedScenes (links (head (scene_ports h))) t)

getStory:: String -> [SceneMeta] -> String
getStory scene_id (h:t)  
    | (link_id h) == scene_id = (text h)
    | otherwise = (getStory scene_id t)

getLinkedScenes:: [String]->[StoryScene] -> [StoryScene]
getLinkedScenes [] _ = []
getLinkedScenes _ [] = []
getLinkedScenes (h:t) (h1:t1) =
    (map (getLinkedScene (h1:t1)) (h:t))
 
getLinkedScene:: [StoryScene] -> String -> StoryScene
getLinkedScene (h:t) link
    | (link `elem` (links (head (scene_ports h)))) = h
    | otherwise = (getLinkedScene t link)
 
display :: Either String Story -> IO()
display s =
    case s of
        Left err -> putStrLn err
        Right story -> print (storyToNode story)



main = parseStory "example_data/cpsc-312-acyclic.json" display
-- (getLinkedScenes ["b0bcbc51-9046-45e6-b38a-80b62f07120b"] (items (story_scenes story)))
-- (getStory "38388378-6f4c-4631-884a-f63ff3ac8fb1" (scene_meta story))

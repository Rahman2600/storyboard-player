import SourceData 

-- Node Title Story Options
data Node = Node String String [Edge] | MtNode deriving (Show, Eq) 

-- Edge Option EndNode
data Edge = Edge String Node deriving (Show, Eq)

-- Story EntryPoint -> Node
storyToNode:: Story -> StoryScene -> Node
storyToNode (Story _ (StoryInfo []) scene_meta) _ = MtNode 
storyToNode (Story _ (StoryInfo storyScenes) scene_meta) entryPoint  = 
    (Node (scene_name entryPoint) (getStory (scene_id entryPoint) scene_meta)
        (map (\sp -> 
            (Edge (label sp) 
                  (storyToNode (Story "" (StoryInfo storyScenes) scene_meta) 
                               (getLinkedScene storyScenes (head (links sp)) (scene_id entryPoint))))) 
        (filter (\sp-> ((not (port_in sp)) && ((length (links sp)) > 0))) (scene_ports entryPoint))))

-- storyToNode:: Story -> StoryScene -> Node
-- storyToNode (Story _ (StoryInfo []) scene_meta) _ = MtNode 
-- storyToNode (Story _ (StoryInfo storyScenes) scene_meta) entryPoint  = 
--     (Node (scene_name entryPoint) (getStory (scene_id entryPoint) scene_meta)
--         (map (\sp -> 
--             (Edge (label sp) 
--                     (storyToNode (Story "" (StoryInfo storyScenes) scene_meta) 
--                                 (getLinkedScene storyScenes (head (links sp)) (scene_id entryPoint))))) 
--         (filter (\sp-> (not (port_in sp))) (scene_ports entryPoint))))
        

-- storyToNode:: Story -> Node
-- storyToNode (Story _ (StoryInfo (h:t)) scene_meta) = 
--     (Node (scene_name h) (getStory (scene_id h) scene_meta)
--         (map (\sp -> 
--             (Edge (label sp) (storyToNode (Story "" (StoryInfo (getLinkedScenes (links sp) (h:t))) 
--         scene_meta)))) 
--         (filter (\sp-> (not (port_in sp))) (scene_ports h))))

getStory:: String -> [SceneMeta] -> String
getStory scene_id (h:t)  
    | (link_id h) == scene_id = (text h)
    | otherwise = (getStory scene_id t)

-- getLinkedScenes:: [String]->[StoryScene] -> [StoryScene]
-- getLinkedScenes [] _ = []
-- getLinkedScenes _ [] = []
-- getLinkedScenes (h:t) (h1:t1) =
--     (map (getLinkedScene (h1:t1)) (h:t))
 
getLinkedScene:: [StoryScene] -> String -> String -> StoryScene
getLinkedScene (h:t) link sceneid
    | (sceneid /= (scene_id h)) &&  (link `elem` (links (head (scene_ports h)))) = h
    | otherwise = (getLinkedScene t link sceneid)
 
display :: Either String Story -> IO()
display s =
    case s of
        Left err -> putStrLn err
        Right story -> print (storyToNode story (head (items (story_scenes (story)))))



main = parseStory "example_data/cpsc-312-acyclic.json" display
-- (getLinkedScenes ["b0bcbc51-9046-45e6-b38a-80b62f07120b"] (items (story_scenes story)))
-- (getStory "38388378-6f4c-4631-884a-f63ff3ac8fb1" (scene_meta story))
-- print (storyToNode story (head (items (story_scenes (story)))))
-- print (getLinkedScene (items (story_scenes story)) "b0bcbc51-9046-45e6-b38a-80b62f07120b" "94913fcb-4d8e-4314-b99e-e24646d5e551")

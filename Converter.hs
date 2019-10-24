module Converter where

import SourceData
import Play


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
 
extractStoryNode :: String -> IO (Maybe Node)
extractStoryNode fp =
    do
        parse <- SourceData.parseStory fp
        case parse of
            Left err -> 
                do
                    putStrLn err
                    return Nothing
            Right story ->
                return (Just (storyToNode story))

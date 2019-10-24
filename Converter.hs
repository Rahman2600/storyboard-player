module Converter where

import SourceData
import Play


storyToNode:: Story -> Node
storyToNode (Story _ (StoryInfo []) scene_meta) = MtNode 
storyToNode (Story _ (StoryInfo (h:t)) scene_meta) = 
    (Node (scene_name h) (getStory (scene_id h) scene_meta)
        (map (\sp -> 
            (Edge (label sp) (storyToNode (Story "" (StoryInfo (getLinkedScenes (links sp) t)) 
        scene_meta)))) 
        (filter (\sp-> (not (port_in sp))) (scene_ports h))))

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

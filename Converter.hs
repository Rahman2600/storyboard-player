module Converter where

-- JSON structure to Story Conversion

import SourceData
import Play

-- Converts a story to node starting from the entryPoint which is where all the other nodes originate from
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
    | (link_id h) == scene_id = (removeHtml (text h))
    | otherwise = (getStory scene_id t)
 
-- removes the outermost part of the html
removeHtml:: String -> String
removeHtml story =  (substring 3 ((length story) - 4) story)

getLinkedScene:: [StoryScene] -> String -> String -> StoryScene
getLinkedScene (h:t) link sceneid
    | (sceneid /= (scene_id h)) &&  (link `elem` (links (head (scene_ports h)))) = h
    | otherwise = (getLinkedScene t link sceneid)

substring :: Int -> Int -> String -> String
substring start end text = take (end - start) (drop start text)
 

-- API

extractStoryNode :: String -> IO (Maybe Node)
-- Consume a file path and attempt to convert to a Node
-- after parsing the file.
-- will produce Nothing if loading, parsing, or converting
-- fails
extractStoryNode fp =
    do
        parse <- SourceData.parseStory fp
        case parse of
            Left err -> 
                do
                    putStrLn err
                    return Nothing
            Right story ->
                return (Just (storyToNode story (head (items (story_scenes (story))))))

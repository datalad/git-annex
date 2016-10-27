commit e233211d1d3bdc844d44f73be37c926393a57571
Author: James MacMahon <jwm@operand.ca>
Date:   Sat Oct 8 01:23:22 2016 -0400

    Drop URL parameters from file extension
    
    During RSS feed importing, some enclosures have URL parameters in them:
    
        http://www.podtrac.com/pts/redirect.mp3/traffic.libsyn.com/classictales/CT_491_The_Blood_is_the_Life.mp3?dest-id=60626
    
    With --template='${feedtitle}/${itempubdate}-${itemtitle}${extension}',
    this is sanitized to
    
        "The_Classic_Tales_Podcast/2016_10_07-Ep._491__The_Blood_Is_The_Life__by_F._Marion_Crawford.mp3_dest_id_60626"
    
    The culprit here is `takeExtension` in Command/ImportFeed.hs:
    
        rundownload url (takeExtension url) $ \f -> do
    
    `takeExtension` is a Posix function that returns the file extension,
    which means all characters after the last '.':
    
        > takeExtension "http://www.podtrac.com/pts/redirect.mp3/traffic.libsyn.com/classictales/CT_491_The_Blood_is_the_Life.mp3?dest-id=60626"
        ".mp3?dest-id=60626"
    
    This commit implements the function dropUrlParameters to take care of
    this:
    
        > dropUrlParameters $ takeExtension "http://www.podtrac.com/pts/redirect.mp3/traffic.libsyn.com/classictales/CT_491_The_Blood_is_the_Life.mp3?dest-id=60626"
        ".mp3"

diff --git a/Command/ImportFeed.hs b/Command/ImportFeed.hs
index 498d504..210aca0 100644
--- a/Command/ImportFeed.hs
+++ b/Command/ImportFeed.hs
@@ -158,10 +158,16 @@ downloadFeed url
                                , return Nothing
                                )
 
+dropUrlParameters :: String -> String
+dropUrlParameters (x:xs) = case x of
+       '?'       -> []
+       otherwise -> [x] ++ dropUrlParameters xs
+dropUrlParameters (x) = x
+
 performDownload :: ImportFeedOptions -> Cache -> ToDownload -> Annex Bool
 performDownload opts cache todownload = case location todownload of
        Enclosure url -> checkknown url $
-               rundownload url (takeExtension url) $ \f -> do
+               rundownload url (dropUrlParameters $ takeExtension url) $ \f -> do
                        r <- Remote.claimingUrl url
                        if Remote.uuid r == webUUID || rawOption opts
                                then do

> Hmm, didn't cleanly apply for some reason. And, `takeWhile (/= '?')` is a 
> simpler way to do that. Thank you for the bug report and patch; [[done]] --[[Joey]]

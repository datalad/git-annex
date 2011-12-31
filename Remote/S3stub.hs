-- stub for when hS3 is not available
module Remote.S3 (remote) where

import Types.Remote
import Types

remote :: RemoteType
remote = RemoteType {
	typename = "S3",
	enumerate = return [],
	generate = error "S3 not enabled",
	setup = error "S3 not enabled"
}

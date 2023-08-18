{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Issue.Comment where

import qualified Data.Time.LocalTime as T
import Domain.Types.Issue.Comment as Comment
import Domain.Types.Issue.IssueReport (IssueReport)
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Logging (Log)
import qualified Sequelize as Se
import qualified Storage.Beam.Issue.Comment as BeamC

create :: (L.MonadFlow m, Log m) => Comment.Comment -> m ()
create = createWithKV

findById :: (L.MonadFlow m, Log m) => Id Comment -> m (Maybe Comment)
findById (Id id) = findOneWithKV [Se.Is BeamC.id $ Se.Eq id]

findAllByIssueReportId :: (L.MonadFlow m, Log m) => Id IssueReport -> m [Comment]
findAllByIssueReportId (Id issueReportId) = findAllWithOptionsKV [Se.Is BeamC.issueReportId $ Se.Eq issueReportId] (Se.Desc BeamC.createdAt) Nothing Nothing

instance FromTType' BeamC.Comment Comment where
  fromTType' BeamC.CommentT {..} = do
    pure $
      Just
        Comment
          { id = Id id,
            issueReportId = Id issueReportId,
            authorId = Id authorId,
            comment = comment,
            createdAt = T.localTimeToUTC T.utc createdAt
          }

instance ToTType' BeamC.Comment Comment where
  toTType' Comment {..} = do
    BeamC.CommentT
      { BeamC.id = getId id,
        BeamC.issueReportId = getId issueReportId,
        BeamC.authorId = getId authorId,
        BeamC.comment = comment,
        BeamC.createdAt = T.utcToLocalTime T.utc createdAt
      }

{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Issues where

import Domain.Types.Issue
import Domain.Types.Person (Person)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Issue

insertIssue :: Issue -> SqlDB ()
insertIssue = do
  Esq.create

findByCustomerId :: Transactionable m => Id Person -> Maybe Int -> Maybe Int -> UTCTime -> UTCTime -> m [Issue]
findByCustomerId customerId mbLimit mbOffset fromDate toDate = do
  Esq.findAll $ do
    issues <- from $ table @IssueT
    where_ $
      issues ^. IssueCustomerId ==. val (toKey customerId)
        &&. issues ^. IssueCreatedAt >=. val fromDate
        &&. issues ^. IssueCreatedAt <=. val toDate
    orderBy [desc $ issues ^. IssueCreatedAt]
    limit limitVal
    offset offsetVal
    pure issues
  where
    limitVal = min (maybe 10 fromIntegral mbLimit) 10
    offsetVal = maybe 0 fromIntegral mbOffset

findAllIssue :: Transactionable m => Maybe Int -> Maybe Int -> UTCTime -> UTCTime -> m [Issue]
findAllIssue mbLimit mbOffset fromDate toDate = do
  Esq.findAll $ do
    issues <- from $ table @IssueT
    where_ $
      issues ^. IssueCreatedAt >=. val fromDate
        &&. issues ^. IssueCreatedAt <=. val toDate
    limit limitVal
    offset offsetVal
    pure issues
  where
    limitVal = min (maybe 10 fromIntegral mbLimit) 10
    offsetVal = maybe 0 fromIntegral mbOffset

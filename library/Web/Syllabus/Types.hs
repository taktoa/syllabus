{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | FIXME: doc
module Web.Syllabus.Types where

import           Control.Lens

import           Data.Text       (Text)
import           Data.Time.Clock (UTCTime)
import           Data.UUID       (UUID)

import           Data.Data       (Data)
import           GHC.Generics    (Generic)

import           Web.Syllabus.TH

-- | A list of items
type List a = [a]

-- | 'Fraction's are floating-point numbers between @0@ and @1@
newtype Fraction = Fraction { _value :: Double }
                 deriving ( Eq, Ord, Num, Enum, Floating, Fractional, Real
                          , RealFloat, RealFrac, Read, Show, Generic, Data )

-- | A grade, as a floating point number between @0@ and @1@
type Grade   = Double
-- | The number of credits, as a floating point number
type Credits = Double
-- | The year, as an integer
type Year    = Int

-- | A policy for dropped assignments.
data DropPolicy = DPDropLowest Int
                  -- ^ Drop a certain number of the lowest grades.

-- | An assignment.
data Assignment = Assignment { _assignmentUUID       :: UUID
                               -- ^ The assignment unique identifier.
                             , _assignmentName       :: Text
                               -- ^ The name of the assignment.
                             , _assignmentDropPolicy :: DropPolicy
                               -- ^ The policy on dropped grades.
                             , _assignmentDueDate    :: UTCTime
                               -- ^ The due date of the assignment.
                             , _assignmentValue      :: Double
                               -- ^ How much is the assignment worth.
                             }

-- | FIXME: doc
data AssignmentGrade = AssignmentGrade { _gradeUUID       :: UUID
                                         -- ^ FIXME: doc
                                       , _gradeValue      :: Double
                                         -- ^ FIXME: doc
                                       , _gradeAssignment :: UUID
                                         -- ^ FIXME: doc
                                       }

-- | A class.
data Class = Class { _classUUID        :: UUID
                     -- ^ The class unique identifier.
                   , _className        :: Text
                     -- ^ The name of the class.
                   , _classCode        :: Text
                     -- ^ The "code" of the class (e.g.: AE 352).
                   , _classDescription :: Text
                     -- ^ Class description.
                   , _classAssignments :: List Assignment
                     -- ^ All the assignments for the given class.
                   , _classCredits     :: Double
                     -- ^ How many credits the class is worth.
                   }

-- | A season, i.e.: winter, spring, summer, or fall.
data Season = SWinter | SSpring | SSummer | SFall

-- | A semester.
data Semester = Semester { _semesterYear    :: Year
                           -- ^ The year in which the semester took place.
                         , _semesterSeason  :: Season
                           -- ^ The season of the semester.
                         , _semesterClasses :: List Class
                           -- ^ The classes taken in this semester.
                         }

deriveSafeCopy 0 'base ''UUID

deriveAll [ ''DropPolicy
          , ''Assignment
          , ''Class
          , ''Season
          , ''Semester ]

UseCaseDiagram: Use Case Diagram
SelectPositionToRankCandidates: Select a position to rank candidates
Docs: Docs
WelcomeTo: Welcome to
EntityRelationshipDiagram: Entity Relationship Diagram
ErDiagram: ER Diagram
BasicEntities: Basic Entities
RecordEdited: Record edited
RecordDeleted: Record deleted
RecordAdded: Record added
NewSkillAdded: New skill added
AdditionalName: Middle name
GivenName: First name
FamilyName: Last name
AppSkillAlreadyExists: The Skill already added for this Applicant
JobSkillAlreadyExists: The Skill already added for this Position
InvalidEndDay: Invalid end date
DuplicateName dupName@Text: Duplicate name: "#{dupName}"
DuplicateCode: Duplicate code
Expand: Expand
Collapse: Collapse
TotalValue totalValue@Text: Total value: #{totalValue}
AggregatedValue v@Text: Aggregated value #{v}
CumulativeWeightNotNormal: Cumulative weight is not in the range 0-1
WeightNotNormal: Weight is not normalized in the range 0-1
Toggle: Toggle
ExpertAssessment: Expert assessment
CalculationAnalysis: Calculation analysis
ShowTop: Show top
Top: Top
FirstN n@Int: First #{show n}
ShowFirstN n@Int: Show first #{show n}
ShowFirst: Show first
Sections: Sections
Photo: Photo
EnterNewValue: Enter new value
NewValue: New value
Age: Age
Attributes: Attributes
Birthday: Birthday
Tag: Tag
Tags: Tags
Label: Label
Labels: Labels
RemoveSubdivision: Remove subdivision
AddSubdivision: Add subdivision
Division: Division
Divisions: Divisions
NumberOfCandidates: Number of candidates
EnterNewName: Enter new name
Category: Category
CreateCategory: Create category
Categories: Categories
InQuotes x@Text: "#{x}"
EditSkills: Edit skills
NSkills: # skills
NumberOfSkills: Number of skills
Breadcrumbs: Breadcrumbs
Undefined: Undefined
Details: Details
NotFound: Not found
Overview: Overview
SeeLink: See
CreatePosition: Create position
EditPosition: Edit position
Actions: Actions
Back: Back
Candidate: Candidate
SelectApplicants: Select applicants
SelectPosition: Select position
SelectAPosition: Select a position
InvalidArguments: Invalid arguments
NumberSign: ##
Position: Position
DayEnd: Date end
DayStart: Date start
Id: Id
Weight: Weight
SelectFromList: Select from list
Select: Select
Close: Close
Patronymic: Patronymic
Surname: Surname
Name: Name
FullName: Full name
Applicants: Applicants
Applicant: Applicant
NoDataFound: No data found
RowsPerPage: Rows per page
Last: Last
Next: Next
Previous: Previous
First: First
PaginationLabel s@Int e@Int n@Int: Record #{show s} - #{show e} of #{show n}
Pagination: Pagination
Clear: Clear
InvalidFormData: Invalid form data
ReallyRemove: Do you really want to remove this record?
ReallyDelete: Do you really want to delete this record?
PleaseConfirm: Please confirm
Remove: Remove
Delete: Delete
Edit: Edit
InputForm: Input form
NewSkill: New skill
Description: Description
Denom: Name
Code: Code
Cancel: Cancel
Save: Save
Add: Add
Search: Search
Skill: Skill
Skills: Skills
Positions: Positions
Candidates: Candidates
Profile: Profile
Exit: Exit
HomePage: Home page
Home: Home
ToggleNavigation: Toggle navigation
AppName: Candidate
MetaKeywords: Ranking, Skills, Aggregation, Hierarchical
MetaDescription: Hierarchical ranking, Selection of candidates based on a hierarchy of attributes, Aggregating hierarchical rankings of skills

Doc1: Candidate Ranking
Doc11: This application provides a simple method for aggregating hierarchical rankings
Doc111: The application helps to make decisions about the selection of candidates based on a hierarchy of attributes, skills and their weight
Doc12: The skills required by a job position should be defined in the dictionary
Doc13: Job positions and their required skills are defined in the section
Doc14: Applicants and their skills are added in the section
Doc15: For each position, the rating of candidates is calculated and the result is available at the link "Candidate"
Doc16: Also in the "Candidates" section, more options are provided for calculating the rating of candidates
Doc18: A Skill is identified by a short code. It has a name and a description. A Skill may be an attribute of a Position and/or of an Applicant.
Doc19: A Position is identified by a code. It is assigned a name, a start date, end date and optionally a division. Also, it has a many-to-many relationship with Skills. Each relationship "Position - Skill" is assigned the weight of the Skill or group. Skills may be grouped. And the groups may be further grouped in a hierarchy.
Doc20: An Applicant is the entity whose Skills will be matched against the Skills required for a Position.
Doc21: A Candidate is an Applicant whose Skills have been compared to those of a particular Position and for whom an aggregate weight (rating) is calculated to distinguish him from other applicants for the same position.
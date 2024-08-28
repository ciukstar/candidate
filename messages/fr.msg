EditApplicant: Modifier le demandeur
NewApplicant: Nouveau demandeur
EditCategory: Modifier la catégorie
BlankValueResetsCategory: Une valeur vide réinitialisera la catégorie
NewSubdivision: Nouveau lotissement
FieldRequired: Le champ est obligatoire
EditDivision: Modifier la division
NewDivision: Nouvelle division
NewPosition: Nouveau poste
EditSkill: Modifier la compétence
EditTheFormAndSavePlease: Veuillez modifier le formulaire et l'enregistrer
FillOutTheFormAndSavePlease: Veuillez remplir le formulaire et l'enregistrer
UseCaseDiagram: Diagramme de cas d'utilisation
SelectPositionToRankCandidates: Sélectionnez une position pour classer les candidats
Docs: Documentation
WelcomeTo: Bienvenue sur
EntityRelationshipDiagram: Diagramme entité-relation
ErDiagram: Diagramme ER
BasicEntities: Entités basiques
RecordEdited: Enregistrement modifié
RecordDeleted: Enregistrement supprimé
RecordAdded: Enregistrement ajouté
NewSkillAdded: Nouvelle compétence ajoutée
AdditionalName: Deuxième nom
GivenName: Prénom
FamilyName: Nom de famille
AppSkillAlreadyExists: La compétence déjà ajoutée pour ce demandeur
JobSkillAlreadyExists: La compétence déjà ajoutée pour ce poste
InvalidEndDay: Date de fin invalide
DuplicateName dupName: Nom en double: "#{dupName}"
DuplicateCode: Code en double
Expand: Expand
Collapse: Réduire
TotalValue totalValue: Valeur totale: #{totalValue}
AggregatedValue v: Valeur agrégée #{v}
CumulativeWeightNotNormal: Le poids cumulé n'est pas compris entre 0 et 1
WeightNotNormal: Le poids n'est pas normalisé dans la plage 0-1
Toggle: Basculer
ExpertAssessment: Appréciation de l'expert
CalculationAnalysis: Analyse de calcul
ShowTop: Afficher le top
Top: Top
FirstN n@Int: Premier #{show n}
ShowFirstN n@Int: Afficher en premier #{show n}
ShowFirst: Afficher en premier
Sections: Sections
Photo: Photo
EnterNewValue: Saisir une nouvelle valeur
NewValue: Nouvelle valeur
Age: Âge
Attributes: Les attributs
Birthday: Anniversaire
Tag: Marquer
Tags: Tags
Label: Label
Labels: Libellés
RemoveSubdivision: Supprimer la subdivision
AddSubdivision: Ajouter une subdivision
Division: Division
Divisions: Divisions
NumberOfCandidates: Nombre de candidats
EnterNewName: Entrez le nouveau nom
Category: Catégorie
CreateCategory: Créer une catégorie
Categories: Catégories
InQuotes x: "#{x}"
EditSkills: Modifier les compétences
NSkills: Qté. compétences
NumberOfSkills: Nombre de compétences
Breadcrumbs: Fil d'ariane
Undefined: Indéfini
Details: Détails
NotFound: Pas trouvé
Overview: Aperçu
SeeLink: Voir le lien
CreatePosition: Créer un poste
EditPosition: Modifier le poste
Actions: Actions
Back: Retournez
Candidate: Candidat
SelectApplicants: Sélectionnez les Demandeurs
SelectPosition: Sélectionnez le poste
SelectAPosition: Sélectionnez un poste
InvalidArguments: Arguments invalides
NumberSign: №
Position: Position
DayEnd: Date de fin
DayStart: Date de début
Id: ID
Weight: Poids
SelectFromList: Sélectionner dans la liste
Select: Sélectionner
Close: Fermer
Patronymic: Patronyme
Surname: Nom de famille
Name: Nom
FullName: Nom complet
Applicants: Demandeurs 
Applicant: Demandeur
NoDataFound: Aucune donnée disponible
RowsPerPage: Lignes par page
Last: Dernière
Next: Suivant
Previous: Précédent
First: Première
PaginationLabel s e n: Enregistrements #{show s} - #{show e} sur #{show n}
Pagination: Pagination
Clear: Effacer
InvalidFormData: Les données du formulaire sont invalides
ReallyRemove: Voulez-vous vraiment supprimer cet enregistrement ?
ReallyDelete: Voulez-vous vraiment supprimer cet enregistrement?
PleaseConfirm: Veuillez confirmer
Remove: Supprimer
Delete: Supprimer
Edit: Modifier
InputForm: Formulaire de saisie
NewSkill: Nouvelle compétence
Description: Description
Denom: Nom
Code: Code
Cancel: Annuler
Save: Sauvegarder
Add: Ajoute
Search: Recherche
Skill: Compétence
Skills: Compétences
Positions: Postes
Candidates: Candidats
Profile: Profil
Exit: Quitter
HomePage: Page d'accueil
Home: Accueil
ToggleNavigation: Basculer la navigation
AppName: Candidat
MetaKeywords: Classement, Compétences, Agrégation, Hiérarchique
MetaDescription: Classement hiérarchique, Sélection des candidats sur la base d'une hiérarchie d'attributs, Aggregating hierarchical rankings of skills

Doc1: Classement des candidats
Doc11: Cette application fournit une méthode simple pour agréger les classements hiérarchiques.
Doc111: L'application aide à prendre des décisions sur la sélection des candidats en fonction d'une hiérarchie d'attributs, de compétences et de leur poids
Doc12: Les Compétences requises par un Poste doivent être définies dans le dictionnaire
Doc13: Les Postes et leurs Compétences requises sont définis dans la section
Doc14: Les Demandeurs et leurs compétences sont ajoutés dans la section
Doc15: Pour chaque Poste, le classement des Candidats est calculé et le résultat est disponible sur le lien "Candidat"
Doc16: Aussi dans la section "Candidats", plus d'options sont fournies pour calculer le classement des Candidats
Doc18: Une Compétence est identifiée par un code court. Il a un nom et une description. Une Compétence peut être un attribut d'un Poste et/ou d'un Demandeur.
Doc19: Un Poste est identifié par un code. On lui attribue un nom, une date de début, une date de fin et éventuellement une division. De plus, il a une relation plusieurs-à-plusieurs avec Compétences. A chaque relation "Poste - Compétence" est attribué le poids de la Compétence ou du groupe. Les Compétences peuvent être regroupées. Et les groupes peuvent être en outre regroupés dans une hiérarchie.
Doc20: Un Demandeur est l'entité dont les Compétences seront mises en correspondance avec les compétences requises pour un Poste.
Doc21: Un Candidat est un Demandeur dont les Compétences ont été comparées à celles d'un Poste particulier et pour lequel un poids global (classement) est calculé pour le distinguer des autres candidats pour le même poste.
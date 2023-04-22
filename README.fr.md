[In English](https://github.com/ciukstar/candidate/blob/master/README.md)  
[На русском](https://github.com/ciukstar/candidate/blob/master/README.ru.md)

# Classement des candidats

Cette application fournit une méthode simple pour agréger les classements hiérarchiques. L'application aide à prendre des décisions sur la sélection des candidats en fonction d'une hiérarchie d'attributs, de compétences et de leur poids.

## Aperçu

Les Compétences requises par un Poste doivent être définies dans le dictionnaire [Compétences](https://candidatefr-biyunm4r4q-de.a.run.app/skills?desc=id&offset=0&limit=5).

Les Postes et leurs Compétences requises sont définis dans la section [Postes](https://candidatefr-biyunm4r4q-de.a.run.app/jobs?desc=id&offset=0&limit=5).

Les Demandeurs et leurs compétences sont ajoutés dans la section [Demandeurs](https://candidatefr-biyunm4r4q-de.a.run.app/applicants?desc=id&offset=0&limit=5).

Pour chaque Poste, le classement des Candidats est calculé et le résultat est disponible sur le lien "Candidat". Voir le lien [Postes](https://candidatefr-biyunm4r4q-de.a.run.app/job-candidates/2).

Aussi dans la section "Candidats", plus d'options sont fournies pour calculer le classement des Candidats. Voir le lien [Candidats](https://candidatefr-biyunm4r4q-de.a.run.app/candidates).

## Entités basiques

### Compétence

Une Compétence est identifiée par un code court. Il a un nom et une description. Une Compétence peut être un attribut d'un Poste et/ou d'un Demandeur.

### Position

Un Poste est identifié par un code. On lui attribue un nom, une date de début, une date de fin et éventuellement une division. De plus, il a une relation plusieurs-à-plusieurs avec Compétences. A chaque relation "Poste - Compétence" est attribué le poids de la Compétence ou du groupe. Les Compétences peuvent être regroupées. Et les groupes peuvent être en outre regroupés dans une hiérarchie.

### Demandeur

Un Demandeur est l'entité dont les Compétences seront mises en correspondance avec les compétences requises pour un Poste.

### Candidat

Un Candidat est un Demandeur dont les Compétences ont été comparées à celles d'un Poste particulier et pour lequel un poids global (classement) est calculé pour le distinguer des autres candidats pour le même poste.

# Démo

[Cliquez ici pour voir la démo](https://candidatefr-biyunm4r4q-de.a.run.app)

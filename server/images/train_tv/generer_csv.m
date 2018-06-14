% genere le .csv pour Amazon Turk avec les images qui sont dans le dossier
% courant
clear;

% Récupérer le nom du dossier courant
currentDir = pwd;
splitter = split(currentDir, '/');
currentDir = splitter(end);

% URL où seront stockés les éléments:
URL = ['https://elystria.github.io/server/images/', currentDir{1}, '/'];

% Créer le fichier et la première ligne
nom_fichier = [currentDir{1}, '.csv'];
fid = fopen(nom_fichier,'wt');
fprintf(fid, 'img_width,img_height,img_url\n');

% Parcourir toutes les images du dossier
listing = dir('*.jpg');
for i=1:size(listing,1)
    
    % Récupérer le nom de l'image
    nom_image = listing(i).name;
    
    I = imread(nom_image);
    [H, W, ~] = size(I);
    
    % Ecrire la largeur et la hauteur, et l'URL
    Hstr = num2str(H);
    Wstr = num2str(W);

    fprintf(fid, [Wstr, ',' , Hstr, ',', URL, nom_image, '\n']);
        
end

fclose(fid);

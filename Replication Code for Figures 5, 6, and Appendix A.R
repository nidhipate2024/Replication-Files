
### --------------------------------------- Armani & Hvidkjaer ---------------------------------------------------###

### ------------------- Wikipedia Scraping and Figures -----------------------###

# Note on replication: To only replicate the paper figures, (i) set directory, (ii) load packages and (iii) skip the scraping replication (rows 36-440). 
# Paper figures start from line 442.

rm(list=ls())

### ----------------------- Working Directory -----------------------###



# ------------------------ Scraping Wiki Pages - Populist Leaders from Funke et al. (2023) ------------------------ #

# Loading Packages #

# Install required packages if not already installed

library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(haven)
library(tidytext)
library(ggplot2)
library(fixest)
library(tidyverse)

# Preparing Data for Scraping #

# List of populist leaders from Funke et al. (2023) 

leaders <- c("Hipólito Yrigoyen",
             "Juan Perón",
             "Isabel Martínez de Perón",
             "Carlos Menem",
             "Néstor Kirchner",
             "Cristina Fernández de Kirchner",
             "Víctor Paz Estenssoro",
             "Hernán Siles Zuazo",
             "Evo Morales",
             "Getúlio Vargas",
             "Fernando Collor",
             "Jair Bolsonaro",
             "Boyko Borisov",
             "Arturo Alessandri",
             "Carlos Ibáñez",
             "José María Velasco Ibarra",
             "Abdalá Bucaram",
             "Rafael Correa",
             "Adolf Hitler",
             "Alexis Tsipras",
             "Viktor Orbán",
             "Indira Gandhi",
             "Narendra Modi",
             "Sukarno",
             "Joko Widodo",
             "Benjamin Netanyahu",
             "Benito Mussolini",
             "Silvio Berlusconi",
             "Junichiro Koizumi",
             "Lázaro Cárdenas",
             "Luis Echeverría",
             "Andrés Manuel López Obrador",
             "Robert Muldoon",
             "Alan García",
             "Alberto Fujimori",
             "Joseph Estrada",
             "Rodrigo Duterte",
             "Jarosław Kaczyński",
             "Vladimír Mečiar",
             "Robert Fico",
             "Jacob Zuma",
             "Roh Moo-hyun",
             "Chen Shui-bian",
             "Thaksin Shinawatra",
             "Recep Tayyip Erdoğan",
             "Boris Johnson",
             "Donald Trump",
             "Hugo Chávez",
             "Nicolás Maduro",
             "Tsai Ing-wen")


# List of non-populist leaders 

# Ideologues Dataset (goes until 2020)

ideologues <- read_dta("ideologues_dataverse_files/identifying_ideologues.dta")

leaders_all <- ideologues$hog %>% unique()

table(ideologues$year) #1945 to 2020


# ------------------------------ Extracting Wikipedia Pages ---------------------------- #

# Helper function to format names for Wikipedia URLs
format_name <- function(name) {
  # Replace spaces with underscores for Wikipedia URLs
  gsub(" ", "_", name)
}

# Base URL for Wikipedia
base_url <- "https://en.wikipedia.org/wiki/"

# Function to scrape content from Wikipedia
scrape_wikipedia <- function(leader) {
  # Format the leader's name for Wikipedia URL
  url <- paste0(base_url, format_name(leader))
  
  # Try to read the page and handle potential errors
  tryCatch({
    # Get the HTML content from the Wikipedia page
    page <- read_html(url)
    
    # Extract the main content (e.g., from <p> tags)
    content <- page %>%
      html_nodes("p") %>%
      html_text(trim = TRUE)
    
    # Combine the paragraphs into a single string
    content <- paste(content, collapse = " ")
    
    # Return the content
    return(content)
    
  }, error = function(e) {
    # Handle error (e.g., if the page doesn't exist)
    message(paste("Could not retrieve page for:", leader))
    return(NA)
  })
}

# Create an empty dataframe to store the results
results <- data.frame(Leader = character(), Content = character(), stringsAsFactors = FALSE)

# Loop over each leader and scrape their Wikipedia page
for (leader in leaders_all) {
  content <- scrape_wikipedia(leader)
  results <- rbind(results, data.frame(Leader = leader, Content = content, stringsAsFactors = FALSE))
}

# Create a check to signal if we need to manually correct the leaders's name/link:
results <- results %>% mutate(need_correction = case_when(grepl("may refer to:", Content) ~ 1,
                                                            TRUE ~ 0))

# Checking unmatched names or the ones returned more than one Wikipedia page to improve the scraping:
correction <- results %>% filter(need_correction ==1) #21 need correction
print(correction)

nas <- results %>% filter(is.na(Content)) # 424


# In results, we have 424 names with NA. So we are going to focus on adjusting those names to the ones most likely to find in Wikipedia:
print(nas$Leader)

simplified_heads_of_government <- c(
  "Mullah Omar", "Ramiz Alia", "Vilson Ahmeti", "Aleksander Meksi", "Pandeli Majko", "Ilir Meta",
  "Lopo do Nascimento", "Fernando van Dunem", "Edelmiro Farrell", "Juan Perón", "Juan Carlos Onganía",
  "Alejandro Lanusse", "Reynaldo Bignone", "Carlos Menem", "Fernando de la Rúa", "Adolfo Rodríguez Saá",
  "Eduardo Duhalde", "Néstor Kirchner", "Gagik Harutyunyan", "Khosrov Harutyunyan", "Hrant Bagratyan",
  "Armen Sargsyan", "Armen Darbinyan", "Aram Sargsyan", "Andranik Margaryan", "Tigran Sargsyan",
  "Karen Karapetyan", "Alfred Sinowatz", "Hasan Hasanov", "Rahim Huseynov", "Surat Huseynov",
  "Fuad Guliyev", "Artur Rasizade", "Khalifa bin Salman", "Salman bin Hamad", "John Adams",
  "Harold St. John", "Vyacheslav Kebich", "Mikhail Chigir", "Sergei Ling", "Vladimir Yermoshin",
  "Henadz Navitski", "Sergey Sidorski", "Mikhail Myasnikovich", "Andrei Kobyakov", "Raman Halouchanka",
  "Achille van Acker", "Gaston Eyskens", "Clovis Pholien", "Jean van Houtte", "Théo Lefèvre",
  "Pierre Harmel", "Paul Vanden Boeynants", "Leo Tindemans", "Guy Verhofstadt", "Yves Leterme",
  "Patrice Talon", "Jigmi Thinley", "Tshering Tobgay", "José Hertzog", "Víctor Paz Estenssoro",
  "Gonzalo Sánchez de Lozada", "Jorge Quiroga", "Carlos Mesa", "Eduardo Rodríguez", "Jeanine Áñez",
  "Mile Akmadžić", "Martin Raguž", "Mokgweetsi Masisi", "Café Filho", "Fernando Collor",
  "Fernando Henrique Cardoso", "Sergey Biryuzov", "Georgi Dimitrov", "Vulko Chervenkov",
  "Petar Mladenov", "Dimitar Popov", "Filip Dimitrov", "Lyuben Berov", "Zhan Videnov",
  "Sergey Stanishev", "Plamen Oresharski", "Maurice Yaméogo", "Sangoulé Lamizana", "Isaac Zida",
  "Christophe Dabiré", "Sein Win", "Norodom Kantol", "Sisowath Sirik Matak", "Carlos Veiga",
  "António do Rosário", "José Maria Neves", "Jean-Bédel Bokassa", "Bernard Ayandho",
  "Jean Koyambounou", "Pahimi Deubet", "Mariano Ospina", "Laureano Gómez", "Alfonso López Michelsen",
  "Belisario Betancur", "Juan Manuel Santos", "Mario Echandi", "Francisco Orlich", "Luis Guillermo Solís",
  "Manuel Varona", "Oscar Gans", "Fulgencio Batista", "Gonzalo Morales", "Spyros Kyprianou",
  "George Vasiliou", "Glafcos Clerides", "Tassos Papadopoulos", "Dimitris Christofias",
  "Nicos Anastasiades", "Leonard Mulumba", "Mpinga Kasenga", "Jean Nguza", "Étienne Tshisekedi",
  "Samy Badibanga", "Bruno Tshibala", "Viggo Kampmann", "Hilmar Baunsgaard", "Anker Jørgensen",
  "Rafael Trujillo", "Joaquín Balaguer", "Héctor García-Godoy", "Antonio Guzmán", "Jorge Blanco",
  "Hipólito Mejía", "Danilo Medina", "Luis Abinader", "Galo Plaza", "Guillermo Rodríguez",
  "León Febres-Cordero", "Rodrigo Borja", "Sixto Durán Ballén", "Fabián Alarcón", "Gustavo Noboa",
  "Lenín Moreno", "Nukrashi Pasha", "Nahhas Pasha", "Oscar Osorio", "José María Lemus",
  "Alfredo Cristiani", "Makhosini Dlamini", "Maphevu Dlamini", "Mabandla Dlamini", "Bhekimpi Dlamini",
  "Jameson Dlamini", "Themba Dlamini", "Aklilu Habte-Wold", "Kamisese Mara", "Mahendra Chaudhry",
  "Frank Bainimarama", "Sakari Tuomioja", "Rainer von Fieandt", "Martti Miettunen", "Ahti Karjalainen",
  "Rafael Paasio", "Teuvo Aura", "Kalevi Sorsa", "Eino Uusitalo", "Léon Blum", "Maurice Couve de Murville",
  "Édith Cresson", "Bessarion Gugushvili", "Tengiz Sigua", "Otar Patsatsia", "Nikoloz Lekishvili",
  "Vazha Lortkipanidze", "Giorgi Arsenishvili", "Avtandil Jorbenadze", "Zurab Zhvania", "Zurab Nogaideli",
  "Kwame Nkrumah", "Themistoklis Sophoulis", "Konstantinos Tsaldaris", "Alexandros Diomidis",
  "Sophoklis Venizelos", "Nikolaos Plastiras", "Alexandros Papagos", "Konstantinos Karamanlis",
  "Georgios Papandreou", "Stefanos Stephanopoulos", "Georgios Papadopoulos", "Georgios Rallis",
  "Xenophon Zolotas", "Konstantinos Mitsotakis", "Kostas Simitis", "Kostas Karamanlis", "Lucas Papademos",
  "Antonis Samaras", "Ramiro de León Carpio", "Otto Pérez Molina", "Alejandro Maldonado",
  "Jimmy Morales", "Manuel Costa", "Alamara Nhasse", "António Sanhá", "Élie Lescot", "Dumarsais Estimé",
  "Claudette Werleigh", "Michèle Pierre-Louis", "Jean-Henry Céant", "Juan Gálvez", "Ramón Villeda Morales",
  "Oswaldo López Arellano", "Policarpo Paz García", "José Azcona", "Carlos Reina", "Roberto Micheletti",
  "H. D. Deve Gowda", "B. J. Habibie", "Megawati Sukarnoputri", "Ahmad Qavam", "Asadullah Alam",
  "Ali Akbar Hashemi Rafsanjani", "Mohammad Khatami", "Hassan Rouhani", "Nuri Said", "Muzahim al-Bajaji",
  "Ali Jawdat", "Ahmad Hasan Khudayir", "Garret FitzGerald", "Gilbert Ake", "Samir al-Rifai",
  "Suleiman Nabulsi", "Bahjat Talhouni", "Wasfi al-Tall", "Hussein Nasir", "Zaid al-Rifai", 
  "Abdul Hamid Sharaf", "Zaid Shaker", "Awn Khasawneh", "Bisher Al-Khasawneh", "Sergey Tereshchenko", 
  "Nurlan Balgimbayev", "Kassym-Jomart Tokayev", "Imangali Tasmagambetov", "Danial Akhmetov", 
  "Karim Massimov", "Serik Akhmetov", "Bakytzhan Sagintayev", "Askar Mamin", "Abdullah III Al-Salim",
  "Sabah Al-Salim", "Aziz Numan", "Jaber Mubarak", "Sabah Al-Khaled", "Tursunbek Chyngyshev",
  "Apas Jumagulov", "Amangeldy Muraliyev", "Igor Chudinov", "Daniyar Usenov", "Omurbek Babanov",
  "Zhantoro Satybaldiyev", "Joomart Otorbayev", "Temir Sariyev", "Sapar Isakov", "Mukhammedkaly Abylgaziev",
  "Prince Boun Oum", "Arturs Krišjānis Kariņš", "Sami Solh", "Riad Solh", "Abdallah al-Yafi",
  "Khaled Chehab", "Takieddin Solh", "Rashid Solh", "Rafik Hariri", "Najib Mikati", "Elias Ramaema",
  "Bethuel Mosisili", "Thomas Thabane", "David Kpormakpor", "Wilton Sankawulo", "Shukri Ghanem", "Khalifa Ghweil", "Fayez al-Sarraj", "Bakili Muluzi",
  "Tun Abdul Razak", "Hussein Onn", "Mahathir Mohamad", "Moulaye Laghdef", "Mohamed Bechir",
  "Miguel Alemán", "Valeriu Muravschi", "Vasile Tarlev", "Vlad Filat", "Yumjaagiin Tsedenbal",
  "Dashiin Byambasuren", "Tsakhiagiin Elbegdorj", "Ukhnaa Khurelsukh", "Mohamed Benhima",
  "Armando Guebuza", "Filipe Nyusi", "Pushpa Kamal Dahal", "Louis Beel", "Joseph Cals", "Joop den Uyl",
  "Violeta Chamorro", "Arnoldo Alemán", "Ahmid Algabid", "Yakubu Gowon", "Usman Shagari",
  "Ljubco Georgievski", "Hussain Suhrawardy", "Yahya Khan", "Miraj Khalid", "Alcibiades Arosemena",
  "Ricardo Arias", "Ernesto de la Guardia", "Mireya Moscoso", "Ricardo Martinelli", "Laurentino Cortizo",
  "Bill Skate", "Higinio Morínigo", "Andrés Rodríguez", "Juan Wasmosy", "Raúl Cubas", 
  "Oscar Duarte", "Luis Franco", "José Odría", "Alan García", "Alberto Fujimori",
  "Valentín Paniagua", "Pedro Pablo Kuczynski", "Martín Vizcarra", "Elpidio Quirino",
  "Ramon Magsaysay", "Joseph Estrada", "Benigno Aquino III", "Jerzy Buzek", "Leszek Miller",
  "Marcelo Caetano", "Francisco Sá Carneiro", "Aníbal Cavaco Silva", "Pedro Santana Lopes",
  "Khalid al-Thani", "Joachim Yhombi-Opango", "Ivan Susaikov", "Mugur Isărescu",
  "Dacian Cioloș", "Joseph Stalin", "Miguel Trovoada", "Celestino Costa", "Daniel Daio",
  "Norberto Costa Alegre", "Carlos Monteiro", "Raul Bragança Neto", "Guilherme Posser",
  "Evaristo Carvalho", "Maria Neves", "Maria Trovoada Silveira", "Tomé Vera Cruz",
  "Gabriel Costa", "Jorge Bom Jesus", "Abdulaziz Ibn Saud", "Rick Houenipwela",
  "Mohamed Abdullahi Mohamed", "Abdulfattah Ismail", "Ali Nasir Muhammad", 
  "Pedro Sánchez", "Jaafar Nimeiry", "Bakri Saleh", "Johannes Kraag", 
  "Runaldo Venetiaan", "Jamil Mardam Bey", "Salah al-Din Bitar", 
  "Nureddin al-Atassi", "Ahmed Khatib", "Mustafa Mero", "Izatullo Khayoyev",
  "Abdumalik Abdullajanov", "Abdujalil Samadov", "Jamshed Karimov", 
  "Rui Araújo", "Edouard Kodjo", "Koffi Adoboli", "Victoire Dogbe",
  "George Chambers", "Patrick Manning", "Keith Rowley", "Şemsettin Günaltay",
  "Ismail Nihat Erim", "Ferit Sadi Melen", "Bülent Ulusu", 
  "Gurbanguly Berdimuhamedow", "Godfrey Binaisa", "Tito Okello",
  "Vitold Fokin", "Yukhym Zvyahilsky", "Vitaliy Masol", 
  "Yevhen Marchuk", "Pavlo Lazarenko", "Valeriy Pustovoitenko",
  "Viktor Yushchenko", "Anatoliy Kinakh", "Yuriy Yekhanurov", 
  "Oleksiy Honcharuk", "Rashid Al Maktoum", "Juan José de Amezaga", 
  "Luis Batlle", "Aparicio Méndez", "Jorge Batlle", "José Mujica",
  "Serge Vohor", "Donald Kalpokas", "Barak Sope", "Edward Natapei",
  "Ham Lini", "Meltek Kilman", "Rómulo Betancourt", "Carlos Delgado Chalbaud",
  "Marcos Pérez Jiménez", "Raúl Leoni", "Jaime Lusinchi",
  "Ramón Velásquez", "Prince Hassan Yahya", "Ahmad IV Shams",
  "Khaled Bahah", "Edgar Lungu",  "Edgar Sanabria", "Abdallah Ibrahim", "Raul Cubas Grau",
  "Edgar Sanabria", "Abdallah Ibrahim", "Raul Cubas Grau") %>% unique()

# Extract the missing 424 now:

# Helper function to format names for Wikipedia URLs
format_name <- function(name) {
  # Replace spaces with underscores for Wikipedia URLs
  gsub(" ", "_", name)
}

# Base URL for Wikipedia
base_url <- "https://en.wikipedia.org/wiki/"

# Function to scrape content from Wikipedia
scrape_wikipedia <- function(leader) {
  # Format the leader's name for Wikipedia URL
  url <- paste0(base_url, format_name(leader))
  
  # Try to read the page and handle potential errors
  tryCatch({
    # Get the HTML content from the Wikipedia page
    page <- read_html(url)
    
    # Extract the main content (e.g., from <p> tags)
    content <- page %>%
      html_nodes("p") %>%
      html_text(trim = TRUE)
    
    # Combine the paragraphs into a single string
    content <- paste(content, collapse = " ")
    
    # Return the content
    return(content)
    
  }, error = function(e) {
    # Handle error (e.g., if the page doesn't exist)
    message(paste("Could not retrieve page for:", leader))
    return(NA)
  })
}

# Create an empty dataframe to store the results
results2 <- data.frame(Leader = character(), Content = character(), stringsAsFactors = FALSE)


for (leader in simplified_heads_of_government) {
  content <- scrape_wikipedia(leader)
  results2 <- rbind(results2, data.frame(Leader = leader, Content = content, stringsAsFactors = FALSE))
}

# Create a check to signal if we need to manually correct the leaders's name/link:

results2 <- results2 %>% mutate(need_correction = case_when(grepl("may refer to:", Content) ~ 1,
                                                            TRUE ~ 0))


# Checking unmatched names or the ones returned more than one Wikipedia page to improve the scraping:

correction2 <- results2 %>% filter(need_correction ==1) #16 need correction
print(correction2)

nas2 <- results2 %>% filter(is.na(Content)) # 68

# Let's unify all the missings we still have:
#results 3 is the scraping with improved names from the 424 nas of results 2:
#21 from results 2 (code was not sure which page to scrape)
#16 from results 3 (code was not sure which page to scrape)
#68 from results 3 (NAs) 

round3 <- bind_rows(correction, correction2, nas2)

# Adjust names to version most likely to appear in Wikipedia (based on LLM models output when we ask how the politicians in the round3 dataset appear on Wiki):

round3_revised_names <- c(
  "Mohammad Yusuf", "Mashiur Rahman", "David Thompson", "Joe Clark", "Jan Fischer",
  "Camilo Ponce Enríquez", "Ibrahima Kassory Fofana", "Bjarni Benediktsson", 
  "Andris Bērziņš", "Ahmed Zaki Yamani", "Igor Lukšić", "Mohammad V of Morocco",
  "Ahmed Osman", "Ruud Lubbers", "Geoffrey Palmer", "Abdullah of Saudi Arabia", 
  "Salman of Saudi Arabia", "Aminata Touré", "Zoran Živković", "Rashid Bakr",
  "Sean Chen", "Eduardo Rodríguez Veltzé", "Sein Win", "Mariano Ospina Pérez", 
  "Gonzalo Aguirre", "Antonio Guzmán", "Jorge Blanco", "Guillermo Rodríguez Lara", 
  "Manuel Espinosa", "Juan Gálvez Durón", "Miguel Alemán", "Ricardo Arias", 
  "Andrés Rodríguez", "Oscar Duarte", "Luis Franco", "Carlos Monteiro de Barros", 
  "Gabriel Costa", "Fernando dos Santos", "Alfred Sinowatz", "Khalifa bin Salman Al Khalifa", 
  "Salman bin Hamad Al Khalifa", "Sergei Ling", "Roman Golovchenko", "Clovis Pholien", 
  "Jean Van Houtte", "José Enrique Hertzog", "António do Rosário", "Jean Koyambounou", 
  "Pahimi Padacké", "Manuel Varona", "Leonard Mulumba", "Mpinga Kasenga", 
  "Jean Nguza Karl-i-Bond", "Nukrashi Pasha", "Nahhas Pasha", "Stefanos Stephanopoulos", 
  "António Artur Sanhá", "José Azcona del Hoyo", "Carlos Roberto Reina", 
  "Asadollah Alam", "Muzahim al-Bajaji", "Ali Jawdat al-Ayubi", "Ahmad Hasan Khudayir",
  "Hussein Nasir", "Zaid Shaker", "Danial Akhmetov", "Abdullah Al-Salim Al-Sabah", 
  "Sabah Al-Salim Al-Sabah", "Aziz Saleh Numan", "Jaber Mubarak Al-Sabah", 
  "Sabah Al-Khaled Al-Sabah", "Mukhammedkaly Abylgaziev", "Prince Boun Oum", 
  "Rashid Solh", "Bethuel Pakalitha Mosisili", "Khalifa al-Ghweil", 
  "Moulaye Ould Mohamed Laghdaf", "Mohamed Salem Ould Bechir", "Ukhnaa Khurelsukh", 
  "Joseph Cals", "Ahmid Algabid", "Shehu Shagari", "Huseyn Shaheed Suhrawardy", 
  "Juan Carlos Wasmosy", "José Manuel Odría", "Sheikh Khalid bin Khalifa Al Thani", 
  "Ivan Susanin", "Celestino Costa", "Guilherme Posser da Costa", 
  "Maria Silveira", "Bakri Hasan Saleh", "Johan Kraag", "Runaldo Ronald Venetiaan", 
  "Mohammad Mustafa Mero", "Édouard Kodjo", "Koffi Adoboli", "Victoire Dogbé", 
  "Nihat Erim", "Ferit Melen", "Juan José de Amézaga", "Luis Batlle", 
  "Meltek Kilman", "Ramón Velásquez", "Prince Hassan bin Yahya", 
  "Ahmad al-Shams"
) %>% unique()

# Extract the improved pages:

# Helper function to format names for Wikipedia URLs
format_name <- function(name) {
  # Replace spaces with underscores for Wikipedia URLs
  gsub(" ", "_", name)
}

# Base URL for Wikipedia
base_url <- "https://en.wikipedia.org/wiki/"

# Function to scrape content from Wikipedia
scrape_wikipedia <- function(leader) {
  # Format the leader's name for Wikipedia URL
  url <- paste0(base_url, format_name(leader))
  
  # Try to read the page and handle potential errors
  tryCatch({
    # Get the HTML content from the Wikipedia page
    page <- read_html(url)
    
    # Extract the main content (e.g., from <p> tags)
    content <- page %>%
      html_nodes("p") %>%
      html_text(trim = TRUE)
    
    # Combine the paragraphs into a single string
    content <- paste(content, collapse = " ")
    
    # Return the content
    return(content)
    
  }, error = function(e) {
    # Handle error (e.g., if the page doesn't exist)
    message(paste("Could not retrieve page for:", leader))
    return(NA)
  })
}

# Create an empty dataframe to store the results
results3 <- data.frame(Leader = character(), Content = character(), stringsAsFactors = FALSE)


for (leader in round3_revised_names) {
  content <- scrape_wikipedia(leader)
  results3 <- rbind(results3, data.frame(Leader = leader, Content = content, stringsAsFactors = FALSE))
}

# Create a check to signal if we need to manually correct the leaders' name/link:

results3 <- results3 %>% mutate(need_correction = case_when(grepl("may refer to:", Content) ~ 1,
                                                            TRUE ~ 0))


# Checking unmatched names or the ones returned more than one Wikipedia page to improve the scraping:

correction3 <- results3 %>% filter(need_correction ==1) #22 need correction
nas3 <- results3 %>% filter(is.na(Content)) # 48

# Let's now unify our Heads of Government Wikipedia pages:

results <- results %>% filter(!is.na(Content)) #1858
results_add <- results2 %>% filter(!is.na(Content)) #356
results_add2 <- results3 %>% filter(!is.na(Content)) #57
df <- bind_rows(results, results_add, results_add2) 
df <- df %>% filter(need_correction==0) #2212

# Let's check for other inconsistencies we may need to filter:
subset_extra_check <- df[grepl("is the name of:", df$Content), ]

df <- anti_join(df, subset_extra_check) #2209 this is the sample we use in this first draft

# We are missing 73 entries out of 2282. We will take care of them in the next steps of this project.

# Saving scraped pages:

write.csv(df, "heads_government_wikipedia_1945_2020.csv")


##### ----------- Section 4.1.2, Figure 5: Subsetting Wiki passages according to policy area and building one figure with 6 groups of policy areas ------------- ###

# To replicate the paper figures, start from here (after loading all packages in the beginning):

# ---------- Loading Data ---------- #

# Load our Heads of Government Wikipedia Pages dataset:

df <- read_csv("heads_government_wikipedia_1945_2020.csv") %>% select(Leader, Content)

# Load the Ideologues dataset for metadata:

ideologues <- read_dta("ideologues_dataverse_files/identifying_ideologues.dta")


# Adding the (non)populist feature to the dataset:

populists <- c("Alvaro Enrique Arzu Irigoyen",
               "Maria Estela Martinez Cartas de Peron",
               "Juan Perón",
               "Carlos Menem",
               "Néstor Kirchner",
               "Cristina Fernandez de Kirchner",
               "Víctor Paz Estenssoro",
               "Hernan Siles Zuazo",
               "Juan Evo Morales Ayma",
               "Getulio Dorneles Vargas",
               "Fernando Collor",
               "Jair Messias Bolsonaro",
               "Boyko Metodiev Borisov",
               "Arturo Alessandri", #out of sample, pre 1945
               "Carlos Ibanez del Campo",
               "Jose Maria Velasco Ibarra",
               "Abdala Jaime Bucaram Ortiz",
               "Rafael Vicente Correa Delgado",
               "Adolf Hitler", #out of sample, 'pre' 1945
               "Alexis Tsipras",
               "Viktor Orban",
               "Indira Priyadarshini Gandhi",
               "Narendra Modi",
               "Sukarno",
               "Joko Widodo",
               "Benjamin Netanyahu",
               "Benito Mussolini", #out of sample, pre 1945
               "Silvio Berlusconi",
               "Junichiro Koizumi",
               "Lazaro Cardenas", #out of sample, pre 1945
               "Miguel Angel Rodriguez Echeverria",
               "Andres Manuel Lopez Obrador",
               "Robert David Muldoon",
               "Alan García",
               "Alberto Fujimori",
               "Joseph Estrada",
               "Rodrigo Roa Duterte",
               "Jaroslaw Aleksander Kaczynski",
               "Vladimir Meciar",
               "Robert Fico",
               "Jacob Gedleyihlekisa Zuma",
               "Roh Moo-hyun",
               "Chen Shui-bian", #out of the sample, it uses premiers and not presidents
               "Thaksin Shinawatra",
               "Recep Tayyip Erdogan",
               "Boris Johnson",
               "Donald John Trump",
               "Hugo Rafael Chavez Frias",
               "Nicolas Maduro",
               "Tsai Ing-wen")

# Adding populist vs. non-populist to the sample of leaders with sentences

df <- df %>% mutate(populist = case_when(Leader %in% populists ~ 1,
                                              TRUE ~ 0))

# -------------- Defining Keywords for Sentiment Analysis ---------------- #

# Define keywords for each policy

# Economic policies demand a first step:

# Define the main terms
main_terms <- c("economy", "economic")

# Define the list of additional words/expressions
additional_terms <- c("policy", "policies", "reform", "reforms", "tax", "taxes", "taxation",
                      "industrial", "industry", "trade", "subsidy", "subsidies", "privatization", 
                      "privatize", "privatized", "nationalization", "nationalize", "nationalized", 
                      "statist", "statism", "statized", "private sector", "public sector", "labor", 
                      "wage", "wages", "budget", "budgetary", "deficit", "spending", "expenditure", 
                      "infrastructure", "income", "finance", "financial", "investment", "invested", 
                      "market", "capital", "exchange rate", "currency", "liberalization", "tariff", 
                      "tariffs", "public asset", "public assets", "state-owned", "regulation", "regulate", 
                      "property rights", "protection", "land", "lands", "security", "labor right", 
                      "labor rights", "health policy", "health policies", "insurance", "inequality", 
                      "social", "accountability", "transparency", "transparent", "accountable")

# Generate all combinations
combinations <- expand.grid(main_terms, additional_terms)

# Combine each pair into a single string and create a vector
combined_vector <- paste(combinations$Var1, combinations$Var2)

# Keywords for each subset of policies:

policy_keywords <- list(
  "Economic Policy" = c(combined_vector),
  "Tax Policy" = c("tax", "taxation", "taxes", "taxed"),
  "Privatization Policy" = c("privatization", "private sector", "state-owned", "public sector", "sell-off", "privatize", "privatized", "private ownership", "public asset", 
                             "government assets", "statism", "nationalize", "nationalization", "statist", "nationalized", "state ownership", "state asset", "state assets", 
                             "state company", "state companies"),
  "Poverty/Inequality Alleviation" = c("poverty", "poor", "low-income", "deprivation", "impoverished", 
                                       "income", "inequality", "wealth", "disparity",  
                                       "financial aid", "food stamps", "low-income families", 
                                       "low-income households", "disadvantaged", "underprivileged", 
                                       "vulnerable groups", "marginalized", "anti-poverty",        "economic inclusion", "homelessness", 
                                       "food insecurity", "nutrition assistance", 
                                       "hunger", "food bank", "redistribution", "redistributive"),
  "Social Rights/Safety Net" = c("welfare", "social safety net", "social support", "social assistance", "social development", "public assistance", 
                                 "labor", "worker", "employment", "wage", "wages", "health", "healthcare", 
                                 "housing",  "insurance", "unemployment"),
  "Property Rights" = c("property", "ownership", "land", 
                        "private asset", "private assets", "patents", "copyright", "trademark", 
                        "real estate",  "expropriation", "expropriated", "expropriate", "seizure", 
                        "tenure", "freehold", "inheritance",  "zoning", "trespass", "legal protection"))

# Function to filter sentences based on keywords

extract_sentences <- function(text, keywords) {
  if (is.null(keywords)) return(text) # No filtering for overall content
  
  sentences <- unlist(strsplit(text, "(?<=[.!?])\\s+", perl = TRUE))
  matched_sentences <- sentences[
    sapply(sentences, function(sentence) sum(sapply(keywords, function(k) grepl(k, sentence, ignore.case = TRUE))) > 0)
  ]
  paste(matched_sentences, collapse = " ")
}

# --------------- Sentiment Analysis using Liu (2012) lexicon ------------------ #

# Empty data frame to store sentiment results
combined_sentiments <- data.frame()

# Loop through each policy and process sentiment
for (policy_name in names(policy_keywords)) {
  keywords <- policy_keywords[[policy_name]]
  
  # Extract relevant sentences for each policy
  df_policy <- df %>%
    mutate(policy_sentences = sapply(Content, extract_sentences, keywords = keywords))
  
  # Tokenize the text data
  tidy_data <- df_policy %>%
    unnest_tokens(word, policy_sentences)
  
  # Join with the Bing sentiment lexicon
  sentiments <- tidy_data %>%
    inner_join(get_sentiments("bing"), by = "word")
  
  # Calculate sentiment score for each leader
  sentiment_scores <- sentiments %>%
    count(Leader, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment_score = positive - negative) %>%
    left_join(df, by = "Leader") %>%
    mutate(policy = policy_name)  
  
 # Adding the (non)populist feature to the dataset:
  
  populists <- c("Alvaro Enrique Arzu Irigoyen",
                 "Maria Estela Martinez Cartas de Peron",
                 "Juan Perón",
                 "Carlos Menem",
                 "Néstor Kirchner",
                 "Cristina Fernandez de Kirchner",
                 "Víctor Paz Estenssoro",
                 "Hernan Siles Zuazo",
                 "Juan Evo Morales Ayma",
                 "Getulio Dorneles Vargas",
                 "Fernando Collor",
                 "Jair Messias Bolsonaro",
                 "Boyko Metodiev Borisov",
                 "Arturo Alessandri", #out of sample, pre 1945
                 "Carlos Ibanez del Campo",
                 "Jose Maria Velasco Ibarra",
                 "Abdala Jaime Bucaram Ortiz",
                 "Rafael Vicente Correa Delgado",
                 "Adolf Hitler", #out of sample, 'pre' 1945
                 "Alexis Tsipras",
                 "Viktor Orban",
                 "Indira Priyadarshini Gandhi",
                 "Narendra Modi",
                 "Sukarno",
                 "Joko Widodo",
                 "Benjamin Netanyahu",
                 "Benito Mussolini", #out of sample, pre 1945
                 "Silvio Berlusconi",
                 "Junichiro Koizumi",
                 "Lazaro Cardenas", #out of sample, pre 1945
                 "Miguel Angel Rodriguez Echeverria",
                 "Andres Manuel Lopez Obrador",
                 "Robert David Muldoon",
                 "Alan García",
                 "Alberto Fujimori",
                 "Joseph Estrada",
                 "Rodrigo Roa Duterte",
                 "Jaroslaw Aleksander Kaczynski",
                 "Vladimir Meciar",
                 "Robert Fico",
                 "Jacob Gedleyihlekisa Zuma",
                 "Roh Moo-hyun",
                 "Chen Shui-bian", #out of the sample, it uses premiers and not presidents
                 "Thaksin Shinawatra",
                 "Recep Tayyip Erdogan",
                 "Boris Johnson",
                 "Donald John Trump",
                 "Hugo Rafael Chavez Frias",
                 "Nicolas Maduro",
                 "Tsai Ing-wen")
  
  # Adding populist vs. non-populist to the sample of leaders with sentences
  
  sentiment_scores <- sentiment_scores %>% mutate(populist = case_when(Leader %in% populists ~ 1,
                                           TRUE ~ 0))
  
  # Summarize the scores for each group
  average_sentiment <- sentiment_scores %>%
    group_by(populist, policy) %>%
    summarize(average_sentiment_score = mean(sentiment_score, na.rm = TRUE), .groups = "drop")
  
  # Append to combined data frame
  combined_sentiments <- bind_rows(combined_sentiments, average_sentiment)
}

# Adjust populist values

combined_sentiments <- combined_sentiments %>% mutate(populist = case_when(populist == 1 ~ "Populist",
                                                                           populist == 0 ~ "Non-Populist"))


# Calculate the average sentiment score difference between Populist and Non-Populist for each policy
policy_differences <- combined_sentiments %>%
  group_by(policy) %>%
  summarize(difference = abs(diff(average_sentiment_score[order(populist)])), .groups = "drop")

# Reorder the policy factor based on the calculated difference
combined_sentiments <- combined_sentiments %>%
  left_join(policy_differences, by = "policy") %>%
  mutate(policy = reorder(policy, difference))

# Create the faceted plot
plot_combined_sentiments <- ggplot(combined_sentiments, aes(x = populist, y = average_sentiment_score, fill = populist)) +
  geom_bar(stat = "identity", color = "black", width = 0.6, show.legend = FALSE) +
  scale_fill_manual(values = c("Non-Populist" = "grey30", "Populist" = "firebrick")) +
  labs(
    title = "Sentiment Analysis of Various Policies of Global Leaders on Wikipedia",
    x = "Type of Leader",
    y = "Average Sentiment Score (Positive - Negative)"
  ) +
  facet_wrap(~ policy) +  # Create facets for each policy
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "black"),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text = element_text(size = 12),
    panel.grid.major.y = element_line(color = "grey85"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "grey98", color = NA)
  ) +
  coord_cartesian(ylim = c(-7, 2)) +
  geom_text(aes(label = round(average_sentiment_score, 2)), 
            position = position_dodge(width = 0.6), 
            vjust = -0.5, 
            size = 4)

# Display the plot
print(plot_combined_sentiments)


# Appendix A - n of Outputs for Populists and Non-Populists

# Summarizing number of leaders who have sentences for each policy area

# Empty data frame to store the number of leaders (populist vs. non-populist) per policy
leader_counts <- data.frame()

# Loop through each policy and process sentiment
for (policy_name in names(policy_keywords)) {
  keywords <- policy_keywords[[policy_name]]
  
  # Extract relevant sentences for each policy
  df_policy <- df %>%
    mutate(policy_sentences = sapply(Content, extract_sentences, keywords = keywords))
  
  # Count the number of unique populist and non-populist leaders with sentences for this policy
  num_leaders <- df_policy %>%
    filter(policy_sentences != "") %>%  # Only count leaders with sentences
    group_by(policy = policy_name, populist) %>%
    summarize(num_leaders = n_distinct(Leader), .groups = "drop")
  
  # Append to the leader counts data frame
  leader_counts <- bind_rows(leader_counts, num_leaders)
  
  # Tokenize the text data for sentiment analysis
  tidy_data <- df_policy %>%
    unnest_tokens(word, policy_sentences)
  
  # Join with the Bing sentiment lexicon
  sentiments <- tidy_data %>%
    inner_join(get_sentiments("bing"), by = "word")
  
  # Calculate sentiment score for each leader
  sentiment_scores <- sentiments %>%
    count(Leader, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment_score = positive - negative) %>%
    left_join(df, by = "Leader") %>%
    mutate(policy = policy_name)  
  
  # Summarize the scores for each group
  average_sentiment <- sentiment_scores %>%
    group_by(populist, policy) %>%
    summarize(average_sentiment_score = mean(sentiment_score, na.rm = TRUE), .groups = "drop")
  
  average_sentiment <- average_sentiment %>% mutate(populist = case_when(populist == 1 ~ "Populist",
                                                                         populist == 0 ~ "Non-Populist"))
  # Append to combined data frame
  combined_sentiments <- bind_rows(combined_sentiments, average_sentiment)
}

# Add descriptive labels for populist status
leader_counts <- leader_counts %>%
  mutate(populist = ifelse(populist == 1, "Populist", "Non-Populist"))

# Print the number of populist and non-populist leaders with sentences for each policy type
print(leader_counts)

# ---------------------- Section 4.1.2, Figure 6: Regressions ------------------------ #
 
# Let's run regressions to map the correlation between being a poopulist and sentiment scores of Wikipedia pages

# Preparing the data:

average_sentiment <- sentiment_scores %>%
  group_by(Leader, policy) %>%
  summarize(
    average_sentiment_score = mean(sentiment_score, na.rm = TRUE),
    count = n()
  )

# Empty list to store sentiment scores for each policy by leader
policy_sentiment_list <- list()

# Loop through each policy to process sentiment scores
for (policy_name in names(policy_keywords)) {
  keywords <- policy_keywords[[policy_name]]
  
  # Extract relevant sentences for each policy
  df_policy <- df %>%
    mutate(policy_sentences = sapply(Content, extract_sentences, keywords = keywords))
  
  # Tokenize the text data
  tidy_data <- df_policy %>%
    unnest_tokens(word, policy_sentences)
  
  # Join with the Bing sentiment lexicon
  sentiments <- tidy_data %>%
    inner_join(get_sentiments("bing"), by = "word")
  
  # Calculate sentiment score for each leader within this policy
  sentiment_scores <- sentiments %>%
    count(Leader, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment_score = positive - negative) %>%
    select(Leader, sentiment_score) %>%
    mutate(policy = policy_name)  # Add policy column
  
  # Append to the list
  policy_sentiment_list[[policy_name]] <- sentiment_scores
}

# Combine all policies' data frames into one
combined_sentiments_long <- bind_rows(policy_sentiment_list)

# Reshape to wide format with one column per policy and NA where no sentence is found
combined_sentiments_wide <- combined_sentiments_long %>%
  spread(key = policy, value = sentiment_score)

# Join with leader metadata from the original data frame
combined_sentiments_wide <- df %>%
  select(Leader, populist) %>%
  distinct() %>%
  left_join(combined_sentiments_wide, by = "Leader")

# Print the resulting data frame
print(combined_sentiments_wide)

# Merge without adding new rows
combined_sentiments_wide <- inner_join(combined_sentiments_wide, ideologues, by = c("Leader" = "hog"))

# Rename policies
combined_sentiments_wide$tax_policy <- combined_sentiments_wide$`Tax Policy`
combined_sentiments_wide$economic_policy <- combined_sentiments_wide$`Economic Policy`
combined_sentiments_wide$privatization_policy <- combined_sentiments_wide$`Privatization Policy`
combined_sentiments_wide$poverty_policy <- combined_sentiments_wide$`Poverty/Inequality Alleviation`
combined_sentiments_wide$social_rights <- combined_sentiments_wide$`Social Rights/Safety Net`
combined_sentiments_wide$property_rights <- combined_sentiments_wide$`Property Rights`


# Run Fixed Effects Regressions for Six Policies: 

# Define the list of policies
policy_columns <- c("economic_policy", "tax_policy", "privatization_policy", 
                    "poverty_policy", "social_rights", "property_rights")

# Initialize a list to store regression results
results <- list()

# Loop through each policy column and run the regression
for (policy in policy_columns) {
  # Create a new column for the current policy's sentiment score
  combined_sentiments_wide[[policy]] <- combined_sentiments_wide[[policy]]
  
  # Run fixed effects regression for the current policy
  model <- feols(as.formula(paste(policy, "~ populist | year + country_name")), data = combined_sentiments_wide)
  
  # Store the results for each policy
  results[[policy]] <- model
}

# Extract the coefficients and confidence intervals for plotting
results_df <- do.call(rbind, lapply(names(results), function(policy) {
  model <- results[[policy]]
  data.frame(
    policy = policy,
    Estimate = coef(model)["populist"],
    CI_Lower = confint(model)["populist", 1],
    CI_Upper = confint(model)["populist", 2]
  )
}))

# Define custom labels for each policy

policy_labels <- c(
  "economic_policy" = "Economic Policy",
  "tax_policy" = "Tax Policy",
  "privatization_policy" = "Privatization Policy",
  "poverty_policy" = "Poverty / Inequality Alleviation",
  "social_rights" = "Social Rights / Safety Net",
  "property_rights" = "Property Rights"
)

# Create the plot with custom facet labels, no left-side labels, and a dotted vertical line at zero
ggplot(results_df, aes(x = Estimate, y = policy)) +
  geom_point(color = "firebrick", size = 5) +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2, color = "grey30") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50", size = 0.5) +  # Add a dotted line at x=0
  labs(
    title = "Effect of Being a Populist on Sentiment Score by Policy Area",
    x = "Effect Estimate (95% CI)"
  ) +
  facet_wrap(~ policy, scales = "free_y", labeller = as_labeller(policy_labels)) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.y = element_blank(),       
    axis.title.y = element_blank(),      
    strip.text.y = element_blank(),      
    axis.text = element_text(size = 12),
    axis.title.x = element_text(size = 14)
  )


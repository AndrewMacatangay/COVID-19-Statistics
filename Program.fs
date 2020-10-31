//Andrew Macatangay

//Some CSV files have names that are updated later on. We need to update these
let getUpdatedCountryName name = 
    if name = "Mainland China" then "China"
    elif name = "United Kingdom" then "UK"
    elif name = "Taiwan*" then "Taiwan"
    elif name = "occupied Palestinian territory" then "Palenstine"
    elif name = "Holy See" then "Vatican City"
    elif name = "Repbulic of the Congo" then "Congo (Brazzaville)"
    //elif name = "Aruba" then "Netherlands"
    else name

//Take contents, parse data, and create a new list
let ParseCSVDatabase lines csvFile =
    //Parse all data to tuples
    let ParseCSVLine (line:string) = 
        let tokens = line.Split(',')
        let listOfValues = Array.toList tokens

        //Create a 4-tuple from the data. Depending on formatting, some conditionals may need changing
        if listOfValues.Length % 2 = 0 || csvFile = "03-23-2020.csv"
        then let _::CountryRegion::_::Confirmed::Deaths::Recovered::_ = listOfValues
             (getUpdatedCountryName CountryRegion, Confirmed, Deaths, Recovered)
        else let _::_::CountryRegion::_::Confirmed::Deaths::Recovered::_ = listOfValues
             (CountryRegion, Confirmed, Deaths, Recovered)
    
    //Map each line to a tuple, then turn the sequence into a list
    let data = Seq.map ParseCSVLine lines
    Seq.toList data

//Open the CDR file (4-tuple)
let openFile csvFile = 
    let contents = System.IO.File.ReadLines(csvFile)
    List.tail (ParseCSVDatabase contents csvFile)

//Take contents, parse data, and create a new list
let ParseCSVDatabaseCountry lines = 
    //Parse all data to tuples
    let ParseCSVLineCountry (line:string) = 
        let tokens = line.Split(',')
        let listOfValues = Array.toList tokens

        let Position::Name::Value::[] = listOfValues
        (Position, Name, Value)

    //Map each line to a tuple, then turn the sequence into a list
    let data = Seq.map ParseCSVLineCountry lines
    Seq.toList data

//Opens any extra files (3-tuple)
let openExtraFile csvFile = 
    let contents = System.IO.File.ReadLines(csvFile)
    List.tail (ParseCSVDatabaseCountry contents)

//Function that allows the programmer to access any one element of the 4-tuple based on index
let getElementOfTupleCDR tuple index = 
    let (CountryRegion, Confirmed, Deaths, Recovered) = tuple
    match index with
    | 0 -> CountryRegion
    | 1 -> Confirmed
    | 2 -> Deaths
    | 3 -> Recovered
    | _ -> ""

//Function that allows the programmer to access any one element of the 3-tuple based on index
let getElementOfTuplePLE tuple index = 
    let (Position, Name, Value) = tuple
    match index with
    | 0 -> Position
    | 1 -> Name
    | 2 -> Value
    | _ -> ""

//Get the total number of countries from all data sets
let rec getNumberOfAllCountries directoryList accumulator =
    //Open file, get countries, append to accumulator, get uniques, and repeat
    match directoryList with
    | [] -> List.length accumulator
    | x::rest -> let data = openFile x |> List.map (fun y -> getElementOfTupleCDR y 0)
                 let acc = (data@accumulator) |> List.distinct
                 getNumberOfAllCountries rest acc

//Allows for commas after each 3 decimal placeS
let printNumber (x : int) : string =
    System.String.Format("{0:N0}",x)

//Converts a string to an int assuming the characters are digits
let stringToInt number =
    match number with
    | "" -> 0
    | number -> int number

//Filter the list of data such that every entry belong to a specific country
let getListOfSameCountry dataCDR country = 
    List.filter (fun x -> getElementOfTupleCDR x 0 = country) dataCDR

//Prints all every country's stats
let printAll dataCDR allCountries = 
    //Takes a country, sums up all it's stats and outputs the result
    let getCountryStats dataCDR country = 
        //Filter the data based on a specific country
        let countryStats = getListOfSameCountry dataCDR country

        //Sum up stats for the country
        let sumConfirmed = List.sumBy (fun x -> stringToInt (getElementOfTupleCDR x 1)) countryStats
        let sumDeaths = List.sumBy (fun x -> stringToInt (getElementOfTupleCDR x 2)) countryStats
        let sumRecovered = List.sumBy (fun x -> stringToInt (getElementOfTupleCDR x 3)) countryStats

        //Print results
        printfn "%s: %s, %s, %s" country (printNumber sumConfirmed) (printNumber sumDeaths) (printNumber sumRecovered)

    List.iter (fun x -> getCountryStats dataCDR x) allCountries

//Get the number of confirmed cases for a specific country
let getCountryCDR dataCDR country option = 
    //Filter the data based on a specific country
    getListOfSameCountry dataCDR country
    |> List.sumBy (fun x-> stringToInt (getElementOfTupleCDR x (option + 1)))

//Get the list of countries and prepare printing the data
let printTopTen dataCDR allCountries = 
    //Map every country to (country, confirmedCases)
    let tuple = List.map (fun x -> (x, getCountryCDR dataCDR x 0)) allCountries
    //Helper function to print the top 10 countries by confirmed cases
    let rec _printTopTen tuple left =
        if left > 0 then
            //Get the highest amount of cases, then remove that country from the list
            let maxTuple = (List.maxBy (fun x -> snd x) tuple)
            let newList = List.filter (fun x -> fst x <> (fst maxTuple)) tuple

            //Print out the country's confirmed cases, then get the next country's data
            printfn "%d. %s: %s" (11 - left) (fst maxTuple) (printNumber (snd maxTuple))
            _printTopTen newList (left - 1)

        //Once we've collected 10 data pieces, then exit the function by printing nothing
        else printf ""
    _printTopTen tuple 10

//Print the total statistics over all countries
let printTotals dataCDR directoryList =
    //Calculate data
    let confirmed = List.sumBy (fun x -> stringToInt (getElementOfTupleCDR x 1)) dataCDR
    let deaths = List.sumBy (fun x -> stringToInt (getElementOfTupleCDR x 2)) dataCDR
    let recovered = List.sumBy (fun x -> stringToInt (getElementOfTupleCDR x 3)) dataCDR

    //Print data
    printfn "As of %s, the world-wide totals are: " (string (List.last directoryList)).[0..9]
    printfn " confirmed: %s" (printNumber confirmed)
    printfn " deaths: %s (%.2f%%)" (printNumber deaths) (float deaths / float confirmed * 100.0)
    printfn " recovered: %s (%.2f%%)" (printNumber recovered) (float recovered / float confirmed * 100.0)

//Check if the country name exists in any for the csv file. True true if it does, otherwise, false
let checkCountryName dataCDR dataP dataLE country =
    if (List.length (getListOfSameCountry dataCDR country)) <> 0 then true
    elif (List.length (List.filter (fun x -> getElementOfTuplePLE x 1 = country) dataP)) <> 0 then true
    elif (List.length (List.filter (fun x -> getElementOfTuplePLE x 1 = country) dataLE)) <> 0 then true
    else false

//Print the population of a country from populations.csv
let rec printPopulation dataCDR country = 
    match dataCDR with
    | [] -> printfn "Population: 0"
    | x::rest -> if getElementOfTuplePLE x 1 = country then printfn "Population: %s" (printNumber (stringToInt (getElementOfTuplePLE x 2)))
                 else printPopulation rest country

//Print the population of a country from life_expectancies.csv
let rec printLifeExpectancy dataCDR country = 
    match dataCDR with
    | [] -> printfn "Life Expectancy: 0.00"
    | x::rest -> if getElementOfTuplePLE x 1 = country then printfn "Life Expectancy: %s years" (getElementOfTuplePLE x 2)
                 else printLifeExpectancy rest country

//Print the data of a country from the most recent CSV file
let rec printLatestData dataCDR country = 
    match dataCDR with
    | [] -> printf ""
    | x::rest -> if getElementOfTupleCDR x 0 = country
                 then printfn "Latest data:"
                      printfn " confirmed: %s" (printNumber (getCountryCDR dataCDR country 0))
                      printfn " deaths: %s " (printNumber (getCountryCDR dataCDR country 1))
                      printfn " recovered: %s " (printNumber (getCountryCDR dataCDR country 2))
                 else printLatestData rest country

//Print the date of first case of a country
let rec printFirstCase directoryList country =
    //Check if the country exists in a CSV file. If it does, output the date, if not, go to the next CSV file
    match directoryList with
    | [] -> printfn "First confirmed case: none"
    | x::rest -> let data = openFile x
                 if (List.length (getListOfSameCountry data country) <> 0)
                 then printfn "First confirmed case: %s" x.[0..9]
                 else printFirstCase rest country
     
//Print the date of first death of a country
let rec printFirstDeath directoryList country =
    match directoryList with
    //Check if the country exists in the CSV file and the number of deaths is more than one.
    //If it does, output the date, if not, go to the next CSV file
    | [] -> printfn "First confirmed death: none"
    | x::rest -> let data = openFile x
                 let countryTuple = (List.filter (fun y -> ((getElementOfTupleCDR y 0 = country) && (getElementOfTupleCDR y 2 <> "" && getElementOfTupleCDR y 2 <> "0"))) data)
                 if (List.length countryTuple) <> 0
                 then if List.isEmpty countryTuple = false
                      then printfn "First confirmed death: %s" x.[0..9]
                      else printFirstDeath rest country
                 else printFirstDeath rest country

//Print out data based on the country's timeline
let printTimeline directoryList country option =
    //Based on the option, print out the command
    if option = 0 then printfn "Confirmed: "
    elif option = 1 then printfn "Deaths: "
    else printfn "Recovered: "

    //Get the index of the first instance of a non-zero entry
    let getListOfSameCountryNonZero dataCDR country option = 
        List.filter (fun x -> (getElementOfTupleCDR x 0 = country) && (stringToInt (getElementOfTupleCDR x (option + 1)) <> 0)) dataCDR

    //Finds the first index of CSV files in which the country appears for looping
    let rec findFirstIndex directoryList country atIndex = 
        match directoryList with
        | [] -> -1
        | x::rest -> let data = openFile x
                     if (List.length (getListOfSameCountryNonZero data country option) <> 0)
                     then atIndex
                     else findFirstIndex rest country (atIndex + 1)

    //Gets the confirmed, deaths, or recovered of a specific country on a specific day
    let getNumbersDaily csvFile country option = 
        let data = openFile csvFile
        let answer = getListOfSameCountry data country

        //Option is offset by 1 when calling getElementOfTupleCDR
        List.sumBy (fun x -> stringToInt (getElementOfTupleCDR x (option + 1))) answer
    
    //Get the index of the first appearance of the country in the CSV files
    let index = findFirstIndex directoryList country 0

    //If the list is empty, then skip this part, otherwise, print the data
    if index <> -1 then 
        if (directoryList.Length - index > 14)
        //Show data from first and last 7 days
        then for x in index .. index + 6 do 
                printfn "%s (day %d): %s" (directoryList.Item x).[0 .. 9] (x - index + 1) (printNumber (getNumbersDaily (directoryList.Item x) country option))
             printfn " .\n .\n ."
             for x in directoryList.Length - 7 .. directoryList.Length - 1 do
                printfn "%s (day %d): %s" (directoryList.Item x).[0 .. 9] (x - index + 1) (printNumber (getNumbersDaily (directoryList.Item x) country option))
        //Show data from all days since the country has data from less than 15 days
        else for x in index .. directoryList.Length - 1 do
                printfn "%s (day %d): %s" (directoryList.Item x).[0 .. 9] (x - index + 1) (printNumber (getNumbersDaily (directoryList.Item x) country option))

//Print the data from a specifc counry
let printCountryData dataCDR dataP dataLE directoryList country = 
    printPopulation dataP country
    printLifeExpectancy dataLE country
    printLatestData dataCDR country
    printFirstCase directoryList country
    printFirstDeath directoryList country

    //Ask the user if they would like to see the timeline for a specfic statistic
    printf "Do you want to see a timeline? Enter c/d/r/n> "
    let option = System.Console.ReadLine()
    match option with
        | "c" -> printTimeline directoryList country 0
        | "d" -> printTimeline directoryList country 1
        | "r" -> printTimeline directoryList country 2
        | "n" -> printf ""
        | _ -> printf ""

//Get country with the most confirmed in the past 24 hours
let getMostConfirmedToday directoryList = 
    //Get CSV files for from yesterday and today
    let today = openFile (List.item (List.length directoryList - 1) directoryList)
    let yesterday = openFile (List.item (List.length directoryList - 2 ) directoryList)

    //Get a list of countries from yesterday's and today's CSV files
    let allCountriesToday = List.map (fun x -> getElementOfTupleCDR x 0) today |> List.distinct |> List.sort
    let allCountriesYesterday = List.map (fun x -> getElementOfTupleCDR x 0) yesterday |> List.distinct |> List.sort

    //Map every country to (country, confirmedCases)
    let getAllConfirmedCases dataCDR allCountries = 
        List.map (fun x -> (x, getCountryCDR dataCDR x 0)) allCountries

    //Get a list of tuples that contains the country name and the number of confirmed cases
    let todayConfirmed = getAllConfirmedCases today allCountriesToday
    let yesterdayConfirmed = getAllConfirmedCases yesterday allCountriesYesterday

    //Checks both lists and gets the difference of confirmed cases to see how many have been reported in the past 24 hours
    let rec getDifference today yesterday = 
        match today with
        | [] -> []
                     //Filter list 2 to see if a matching country name exists
        | e::rest -> let newList = List.filter (fun x -> fst e = fst x) yesterday
                        
                     //If a matching name exists or a new country appears in the most recent CSV file, then get append the tuple to the list, otherwise, get the next country
                     if (newList.Length <> 0)
                     then if snd e - snd newList.Head >= 0
                          then (fst e, snd e - snd newList.Head)::(getDifference rest yesterday)
                          else getDifference rest yesterday
                     else (fst e, snd e)::(getDifference rest yesterday)

    //Get a tuple of the confirmed cases in the past day and sort the data
    let confirmedInPastDay = getDifference todayConfirmed yesterdayConfirmed
    let confirmedInPastDaySorted = List.sortBy (fun x -> -snd x) confirmedInPastDay

    //Print the results
    for x in 0 .. confirmedInPastDaySorted.Length - 1 do
        printfn "%d. %s: %s" (x + 1) (fst confirmedInPastDaySorted.[x]) (printNumber (snd confirmedInPastDaySorted.[x]))

[<EntryPoint>]
let main argv = 
    //Get list of all .csv files with CDR. Use "filesNamesOld.txt" for 1-22-2020 to 3-18-2020
    let directoryArray = System.IO.File.ReadLines("fileNames.txt")
    let directoryList = Seq.toList directoryArray |> List.map (fun x -> x + ".csv")

    //Read in latest file and parse it's contents
    let filename = List.head (List.rev directoryList)
    let dataCDR = openFile filename

    //Read in population and life expenctancy file ands parse their contents
    let dataP = openExtraFile "populations.csv"
    let dataLE = openExtraFile "life_expectancies.csv"

    printfn "** COVID-19 Data Analysis **\n"
    printfn "Based on data made available by John Hopkins University"
    printfn "https://github.com/CSSEGISandData/COVID-19\n"

    printfn ">> Processed %d daily reports" (List.length directoryList)
    printfn ">> Processed %d files of world facts" 2
    printfn ">> Current data on %d countries\n" (getNumberOfAllCountries directoryList [])

    //Returns a list of unique country names in sorted order
    let allCountries = List.map (fun x -> getElementOfTupleCDR x 0) dataCDR |> List.distinct |> List.sort

    //The status of the loop is mutable
    let mutable continueLoop = true

    //Prompt the user for command
    printf "Enter command (help for list, # to quit)> "
    let mutable command = System.Console.ReadLine()
    if command = "#" then continueLoop <- false

    //While the input is not "#", continue loop
    while continueLoop = true do
        match command with
        | "help" -> printfn " <name>: enter a country name such as US or China"
                    printfn " countries: list all countries and most recent report"
                    printfn " top10: list of top 10 countries based on most recent # of confirmed cases"
                    printfn " totals: world-wide totals of confirmed, deaths, recovered"
                    printfn " past24hours: list the number of cases of each country in the past 24 hours"
        | "countries" -> printAll dataCDR allCountries
        | "top10" -> printTopTen dataCDR allCountries
        | "totals" -> printTotals dataCDR directoryList
        | "past24hours" -> getMostConfirmedToday directoryList
        | "#" -> continueLoop <- false
        | x -> if (checkCountryName dataCDR dataP dataLE x) = true then printCountryData dataCDR dataP dataLE directoryList x
               else printfn "country or command not found..."

        //Prompt the user for the next command
        if continueLoop = true then
            printf "\nEnter command> " 
            command <- System.Console.ReadLine()
    0
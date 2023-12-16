// Programming Language: F#
// Please run this F# code in this given website "https://paiza.io/en/projects/new?language=fsharp"
// Click Run(Ctrl-Enter) button to run the code
// Program Objective: To calculate minimum cost for kayak allocation, and show the distribution.
// Program Input: A set of 100 students, with their weight in the range 100 and 250.
// Program Output: Minimum cost required for kayaks, and its allocation.
open System
// Create an object Student, with ID and weight as its attributes.
type Student = { ID: int; studentWeight: int }
// The function generates a random weight value between 100 and 250.
let generateWeight () =
    let random = Random()
    (random.Next(7, 16)) * 15

// This function maps the student with the weight.
let generateStudent id =
    { ID = id
      studentWeight = generateWeight () }

// The below function generates a list of students.
let generateStudents () = [ 1..100 ] |> List.map generateStudent

// ******
// Procudure: allocateStudentsToKayaks
// Purpose: This function allocates/maps the different students to the respective
//          kayaks, based on the given constraints.
// ******

let allocateStudentsToKayaks students =
    let sortedStudentsWeight = List.sortByDescending (fun s -> s.studentWeight) students
    // The below sub procedure is called recursively to allocate students
    // to kayaks.
    let rec allocate acc kayaks remainingStudent =
        match remainingStudent with
        | [] -> acc
        | s :: variableName ->
            //Assigns the kayak to students
            let assignkayak =
                match List.tryFindIndex (fun k -> k + s.studentWeight <= 300) kayaks with
                | Some(variable) -> variable
                | None -> List.length kayaks // Add a new kayak

            let updatedKayaks =
                if assignkayak = List.length kayaks then
                    kayaks @ [ s.studentWeight ]
                else
                    List.mapi (fun i k -> if i = assignkayak then k + s.studentWeight else k) kayaks

            let updatedStudents = (s.ID, s.studentWeight, assignkayak + 1) :: acc
            allocate updatedStudents updatedKayaks variableName

    allocate [] [] sortedStudentsWeight

// This prints the calculated values for kayak distribution.
let printResult (id, weight, kayak) =
    printfn "Student ID: %d (Weight: %d) -> Kayak ID: %d" id weight kayak

// This stores the instance of kayak distribution values.
let printResults allocatedStudents =
    allocatedStudents
    |> List.sortBy (fun (_, _, kayak) -> kayak)
    |> List.iter printResult

// The below function gets the total number of kayaks utilized.
let getMaxKayakID allocatedStudents =
    allocatedStudents |> List.map (fun (_, _, kayak) -> kayak) |> List.max

// The below function gets the total cost for the kayaks utilized.
let calculateInvestmentCost allocatedStudents =
    let maxKayakID = getMaxKayakID allocatedStudents
    maxKayakID * 20

let displayInvestmentInfo totalCost =
    printfn "Minimum amount of dollars GWU would have to invest: $%d" totalCost

let main () =
    printfn "The students are generated with individual and unique ID's."

    printfn
        "Random weights are generated for every students and then the kayak number is assigned to the students based on their weights."

    let students = generateStudents () // Generate students and associate weights to them.
    let allocatedStudents = allocateStudentsToKayaks students // Perform kayak allocation.
    let totalCost = calculateInvestmentCost allocatedStudents // Calculate the minimum cost.
    displayInvestmentInfo totalCost
    printfn "Students and their assigned kayaks:"
    printResults allocatedStudents

main ()

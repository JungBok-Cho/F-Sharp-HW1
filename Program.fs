(* JungBok Cho
   F# Assignment 1
*)

open System

/// <Part 1> - maxCubeVolume
/// maxCubeVolume fucntion takes a list of floating-point tuples 
/// that represent dimensions of a cube (length, width, and height) 
/// and returns the volume of the cube that has the largest volume.
/// If the list is empty, it will return 0.0.
/// 
/// For example, it will work like this: 
/// _arg1:(float * float * float) list -> float
let rec maxCubeVolume = function
    | [] -> 0.0
    | (a, b, c) :: tl -> 
        let volume = a * b * c
        let maxVolume = maxCubeVolume tl
        if volume > maxVolume then volume
        else maxVolume


/// <Part 2> - findMatches
/// findMatches function takes a string and a list of tuples as arguments. 
/// Each element of the list will be a tuple consisting of a string and an int. 
/// Then, it finds all of the tuples for which the string matches the first argument 
/// and collect all of the corresponding integers. Then, it will return the 
/// list of the collection. However, if the given list is empty, it will return empty list.
/// 
/// For example, it will work like this: 
/// target:'a -> _arg1:('a * int) list -> int list when 'a : equality
let rec findMatches target = function
    | [] -> [] : int list
    | (a, b) :: tl ->
        let temp = findMatches target tl
        if a = target then 
            b :: temp
        else
            temp


/// <Part 3> - BST
/// 
/// Tree definition for Part 3
type BST =
    | Empty
    | TreeNode of int * BST * BST

/// Inserts the value into the tree and returns the resulting tree
let rec insert value tree = 
    match tree with
    | Empty -> TreeNode (value, Empty, Empty)
    | TreeNode(v, l, r) -> 
        if value > v then 
            TreeNode (v, l, insert value r)
        else 
            TreeNode (v, insert value l, r)

/// Returns true if the value is in the tree or false if it is not.
let rec contains value tree = 
    match tree with
    | Empty -> false
    | TreeNode(v, l, r) -> 
        if value > v then 
            contains value r
        elif value < v then 
            contains value l
        else
            true

/// The parameter func is a Boolean function that takes a single parameter 
/// and returns true or false. The function tests the value of each node 
/// with func and returns the number of nodes that evaluate to true.
let rec count func tree = 
    match tree with 
    | Empty -> 0
    | TreeNode(v, l, r) ->
        let result = (count func l) + (count func r)
        if func v then result + 1
        else result

/// Returns the number of nodes that contain even integers.
let evenCount tree = count (fun x -> x % 2 = 0) tree


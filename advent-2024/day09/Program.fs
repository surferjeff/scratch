open System.IO

let input = File.ReadAllLines("input.txt")
type Block =
| Free
| File of int

let rec addFileBlocks (line: string) (i: int) (fileNumber: int) (disk: ResizeArray<Block>) =
    if i >= line.Length then
        disk
    else
        let count = int (line[i] - '0')
        for i in 0..count-1 do
            disk.Add(File fileNumber)
        addFreeBlocks line (i + 1) (fileNumber + 1) disk

and addFreeBlocks (line: string) (i: int) (fileNumber: int) (disk: ResizeArray<Block>) =
    if i >= line.Length then
        disk
    else
        let count = int (line[i] - '0')
        for i in 0..count-1 do
            disk.Add(Free)
        addFileBlocks line (i + 1) fileNumber disk

let disk = addFileBlocks input[0] 0 0 (ResizeArray())

let rec compact (disk: ResizeArray<Block>) (starti: int) (endi: int) =
    if starti >= endi then
        ()
    else
        match disk[starti], disk[endi] with
        | Free, File n ->
            disk[starti] <- File n
            disk[endi] <- Free
            compact disk (starti + 1) (endi - 1)
        | _, Free -> compact disk starti (endi - 1)
        | File _, _ -> compact disk (starti + 1) endi

compact disk 0 (disk.Count-1)

let checksum someDisk = 
    someDisk
    |> Array.mapi (fun i block ->
    match block with
    | Free -> 0L
    | File n -> (int64 n) * (int64 i)) |> Array.sum

printfn "part1: %d" (disk.ToArray() |> checksum)

/////////////////////////////////////////////////////////////////////////////
// part2

type FileChunk = {
    diskOffset: int
    size: int
    fileNumber: int
}

// Maps size to disk offset.
type FreeChunks = Map<int, int list>
let addFreeChunk size offset (freeChunks: FreeChunks) =
    freeChunks
    |> Map.change size (function
        | None -> Some [offset]
        | Some lst -> Some (offset :: lst))

let reverseOffsets (freeChunks: FreeChunks) =
    freeChunks
    |> Map.toSeq
    |> Seq.map (function (size, offsets) -> (size, List.rev offsets))
    |> Map.ofSeq

let rec takeFreeChunk size takeSize (freeChunks: FreeChunks) =
    if takeSize = 10
        then freeChunks, None
    else
        match Map.tryFind takeSize freeChunks with
        | None -> takeFreeChunk size (takeSize + 1) freeChunks
        | Some [] -> takeFreeChunk size (takeSize + 1) freeChunks
        | Some (offset :: rest) ->
            let freeChunks = freeChunks |> Map.change takeSize (fun _ -> Some rest)
            if size = takeSize then
                freeChunks, Some offset
            else
                // Put the left over chunk back into freeChunks
                let freeSize = takeSize - size
                let freeChunks = freeChunks |> Map.change freeSize (function
                    | None -> Some [offset + size]
                    | Some offsets -> Some (List.sort((offset + size) :: offsets)))
                freeChunks, Some offset

let rec accFileChunks (line: string) (i: int) (fileNumber: int) (diskOffset: int) (fileChunks: FileChunk list) (freeChunks: FreeChunks) =
    if i >= line.Length then
        fileChunks, freeChunks
    else
        let size = int (line[i] - '0')
        let chunk = {
            fileNumber = fileNumber
            size = size
            diskOffset = diskOffset
        }
        accFreeChunks line (i + 1) (fileNumber + 1) (diskOffset + size) (chunk :: fileChunks) freeChunks

and accFreeChunks (line: string) (i: int) (fileNumber: int) (diskOffset: int) (fileChunks: FileChunk list) (freeChunks: FreeChunks) =
    if i >= line.Length then
        fileChunks, freeChunks
    else
        let size = int (line[i] - '0')
        accFileChunks line (i + 1) fileNumber (diskOffset + size) fileChunks (
            addFreeChunk size diskOffset freeChunks)

let rec compactChunks (fileChunks: FileChunk list) (freeChunks: FreeChunks) (disk: Block array) =
    match fileChunks with
    | [] -> ()
    | lastFile :: restFiles ->
        match takeFreeChunk lastFile.size lastFile.size freeChunks with
        | freeChunks, None -> 
            for i in lastFile.diskOffset..lastFile.diskOffset+lastFile.size-1 do
                disk[i] <- File lastFile.fileNumber
            compactChunks restFiles freeChunks disk
        | freeChunks, Some offset ->
            for i in offset..offset+lastFile.size-1 do
                disk[i] <- File lastFile.fileNumber
            compactChunks restFiles freeChunks disk

let part2 (input: string) = 
    let fileChunks, freeChunks =
        let files, frees = accFileChunks input 0 0 0 [] Map.empty
        files, (reverseOffsets frees)

    let chunkyDisk = Array.create disk.Count Free
    compactChunks fileChunks freeChunks chunkyDisk
    printfn "part2: %d" (checksum chunkyDisk)

part2 input[0]
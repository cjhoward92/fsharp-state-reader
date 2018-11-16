module Config

open Reader

type Config = {
    SourceFile: string;
    DestFile: string;
    MaxLength: int;
    Delimiter: char;
    }

let loadSource loadFn =
    reader {
        let! env = ask
        return loadFn env.SourceFile
    }

let transformSource (srcText: string) =
    let lines = srcText.Split "\n"

    let transformLine (maxLength: int) (delimiter: char) (line: string) =
        let shorten s = if String.length s > maxLength then s.Substring(0, maxLength - 1) else s
        let folder l s = 
            if String.length l > 0 then l + delimiter
            l + (shorten s)

        line.Split delimiter
        |> Array.toList
        |> List.fold (folder) "" 

    let parseLine (line: string) = reader {
        let! env = ask
        return line.Split env.Delimiter |> Array.toList
    }

    Array.toList lines
    |> Reader.traverse parseLine 
    |> Reader.apply (reader {
        let! c = ask
        return List.map (transformLine c.MaxLength c.Delimiter)
    })

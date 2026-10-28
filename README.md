# Laboratorijas darbs Nr. 1 — Haskell (8. variants)

Visi uzdevumi ir apvienoti vienā programmā ar galveno izvēlni.

```
st85980-lab1/
├── Main.hs          galvenā izvēlne
├── Tasks/           uzdevumi Nr. 1–8 (Task1.hs … Task8.hs)
├── Utils/
│   ├── Input.hs     droša ievade un tās pārbaude (robežgadījumi)
│   └── Testing.hs   testu izdruka (Test / Input / Expected / Actual / Result)
└── hie.yaml         iestatījumi VS Code Haskell paplašinājumam
```

## Palaišana

No mapes `lab1`:

```
runghc Main.hs
```

vai kompilējot:

```
ghc Main.hs -o lab1
./lab1
```

Izvēlnē ievadiet uzdevuma numuru (1–8), 0 — iziet. Katrs uzdevums vispirms izdrukā
testus, pēc tam ļauj ievadīt savus datus. Nekorekta ievade (burti skaitļa vietā,
tukša ievade u. c.) netiek pieņemta — programma lūdz ievadīt vēlreiz.

Windows: ja garumzīmes rādās nepareizi, pirms palaišanas izpildiet `chcp 65001`.

### VS Code

Atveriet projekta mapi (File → Open Folder), atveriet termināli (Terminal → New Terminal)
un izpildiet `runghc Main.hs`. Programma gaida ievadi, tāpēc to jāpalaiž terminālī,
nevis ar pogu Run vai paplašinājumu Code Runner.

Papildu bibliotēkas nav vajadzīgas — pietiek ar standarta GHC instalāciju.

`output.txt` ir izveidots, ievadi padodot no faila, tāpēc tajā nav redzami paši
ievadītie teksti — tikai programmas atbildes.

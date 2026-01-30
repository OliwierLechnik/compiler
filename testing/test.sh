#!/bin/bash

# --- KONFIGURACJA ---
COMPILER="./compiler" # KOMENDA DO KOMPILACJI
VM="./vm"  # ŚCIEŻKA DO MASZYNY
TEST_DIR="./test_programs" # FOLDER NA PROGRAMY TESTOWE .IMP
PERF_DIR="./tests"         # FOLDER NA PLIKI Z CASE'AMI DO TESTÓW
OUT_DIR="./out"            # FOLDER NA SKOMPILOWANE PROGRAMY
TIMEOUT_SEC="10s"           # CZAS PO KTÓRYM PROGRAM SIĘ TIMEOUTUJE

# --- KOLORY DO RAPORTU ---
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

mkdir -p "$OUT_DIR"

declare -a REPORT_NAMES
declare -a REPORT_LENS
declare -a REPORT_AVG_COSTS
declare -a REPORT_STATUS

echo -e "${CYAN}=== ROZPOCZYNAM TESTY AUTOMATYCZNE (Timeout: $TIMEOUT_SEC) ===${NC}"

for test_file in "$PERF_DIR"/*; do
    [ -f "$test_file" ] || continue
    filename=$(basename "$test_file")
    if [[ "$filename" == .* ]]; then continue; fi

    # 1. Odczyt nazwy pliku źródłowego
    src_filename=$(head -n 1 "$test_file" | tr -d '\r' | xargs)
    if [ -z "$src_filename" ]; then continue; fi

    src_path="$TEST_DIR/$src_filename"
    compiled_path="$OUT_DIR/${src_filename%.*}.mr"

    echo -e "\n---------------------------------------------------"
    echo -e "Test: ${YELLOW}$filename${NC} -> Źródło: $src_filename"

    if [ ! -f "$src_path" ]; then
        echo -e "${RED}BŁĄD: Nie znaleziono pliku źródłowego: $src_path${NC}"
        REPORT_NAMES+=("$filename")
        REPORT_LENS+=("0")
        REPORT_AVG_COSTS+=("0")
        REPORT_STATUS+=("MISSING")
        continue
    fi

    # 2. Kompilacja
    echo -n "Kompilacja... "
    compiler_out=$($COMPILER "$src_path" "$compiled_path" 2>&1)
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}OK${NC}"
    else
        echo -e "${RED}BŁĄD KOMPILACJI${NC}"
        echo "$compiler_out"
        REPORT_NAMES+=("$filename")
        REPORT_LENS+=("0")
        REPORT_AVG_COSTS+=("0")
        REPORT_STATUS+=("COMPILE_ERR")
        continue
    fi

    total_cost=0
    test_count=0
    code_length=0
    file_status="OK"

    mapfile -t lines < <(tail -n +2 "$test_file")
    current_inputs=""
    current_expects=""

    run_test_case() {
        if [ -z "$current_expects" ]; then return; fi

        test_count=$((test_count + 1))

        vm_input=$(echo "$current_inputs" | tr ' ' '\n')

        # Uruchomienie VM
        vm_output=$(echo -e "$vm_input" | timeout "$TIMEOUT_SEC" $VM "$compiled_path" 2>&1)
        exit_code=$?

        if [ $exit_code -eq 124 ]; then
            echo -e "  Test #${test_count}: ${RED}TIMEOUT${NC}"
            file_status="FAIL"
            return
        fi

        # --- PARSOWANIE (Fix ANSI colors + Strict Regex) ---

        # 1. Liczba rozkazów
        if [ "$code_length" -eq 0 ]; then
            len=$(echo "$vm_output" | sed 's/\x1b\[[0-9;]*m//g' | grep "liczba rozkazów:" | sed 's/[^0-9]*//g')
            if [ ! -z "$len" ]; then code_length=$len; fi
        fi

        # 2. Koszt (Najważniejsza poprawka)
        # Krok 1: sed usuwa kody kolorów (np. \033[31m)
        # Krok 2: grep szuka linii z "koszt:"
        # Krok 3: sed wycina wszystko między "koszt:" a ";" (grupa \1)
        # Krok 4: tr usuwa spacje z liczby (89 334 -> 89334)
        cost=$(echo "$vm_output" | sed 's/\x1b\[[0-9;]*m//g' | grep "koszt:" | sed -n 's/.*koszt: \([^;]*\);.*/\1/p' | tr -d ' ')

        cost=$(echo "$cost" | tr -d ',') # rwlodarczyk fix

        if [[ "$cost" =~ ^[0-9]+$ ]]; then
            total_cost=$((total_cost + cost))
            cost_str="$cost"
        else
            cost_str="???"
        fi

        # 3. Weryfikacja wyniku
        actual=$(echo "$vm_output" | grep -oP ">\s*\K-?[0-9]+" | tr '\n' ' ' | xargs)
        expected=$(echo "$current_expects" | tr '\n' ' ' | xargs)

        if [ "$actual" == "$expected" ]; then
            echo -e "  Test #${test_count}: ${GREEN}PASSED${NC} (Koszt: $cost_str)"
        else
            echo -e "  Test #${test_count}: ${RED}FAILED${NC}"
            echo -e "    Oczekiwano: [$expected]"
            echo -e "    Otrzymano:  [$actual]"
            file_status="FAIL"
        fi
    }

    for line in "${lines[@]}"; do
        line=$(echo "$line" | tr -d '\r')
        if [[ "$line" =~ ^\? ]]; then
            if [ ! -z "$current_expects" ]; then run_test_case; current_inputs=""; current_expects=""; fi
            val=${line#? }; current_inputs="$current_inputs $val"
        elif [[ "$line" =~ ^\> ]]; then
            val=${line#> }; current_expects="$current_expects $val"
        elif [[ -z "$line" ]]; then
            if [ ! -z "$current_expects" ]; then run_test_case; current_inputs=""; current_expects=""; fi
        fi
    done
    if [ ! -z "$current_expects" ]; then run_test_case; fi

    avg_cost=0
    if [ "$test_count" -gt 0 ]; then avg_cost=$((total_cost / test_count)); fi

    REPORT_NAMES+=("$filename")
    REPORT_LENS+=("$code_length")
    REPORT_AVG_COSTS+=("$avg_cost")
    REPORT_STATUS+=("$file_status")
done

echo -e "\n\n${CYAN}=== PODSUMOWANIE ===${NC}"
printf "%-40s | %-10s | %-15s | %-10s\n" "Plik" "Rozkazy" "Śr. Koszt" "Status"
echo "---------------------+------------+-----------------+----------"
for i in "${!REPORT_NAMES[@]}"; do
    s=${REPORT_STATUS[$i]}
    color=$GREEN
    if [ "$s" != "OK" ]; then color=$RED; fi
    printf "%-40s | %-10s | %-15s | ${color}%-10s${NC}\n" "${REPORT_NAMES[$i]}" "${REPORT_LENS[$i]}" "${REPORT_AVG_COSTS[$i]}" "$s"
done
echo ""

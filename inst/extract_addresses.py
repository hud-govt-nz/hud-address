#!/bin/python3
# No goddamn way I'm rewriting this in R - between f-string regex, R's double
# slash and dictionaries, this will be absolutely madshit bonkers to write in R
import re

#===================#
#   Number/street   #
#===================#
# Official NZ Post abbreviations (https://www.nzpost.co.nz/personal/sending-in-nz/how-to-address-mail)
# All the alternates are added in manually cos hey, who actually follows the official ones
STREETS = {
    "Alley": "Aly",
    "Arcade": "Arc",
    "Avenue": "Ave",
    "Bank": "Bank",
    "Bay": "Bay",
    "Beach": "Bch",
    "Belt": "Belt",
    "Bend": "Bnd",
    "Boulevard": "Blvd",
    "Circle": "Cir",
    "Claim": "Clm",
    "Close": "Cl",
    "Common": "Cmn",
    "Court": "Crt|Ct",
    "Courts": "Crts",
    "Cove": "Cv",
    "Creek": "Crk",
    "Crescent": "Cres|Cr|Crs",
    "Crest": "Crst",
    "Dell": "Del",
    "Downs": "Downs",
    "Drive": "Drv|Dr",
    "Esplanade": "Esp",
    "Farms": "Frms",
    "Flat": "Flt",
    "Garden": "Gdn",
    "Gardens": "Gdns",
    "Gate": "Gte",
    "Glade": "Gld|Gl",
    "Glen": "Gln",
    "Green": "Grn",
    "Greens": "Grns",
    "Grove": "Grv|Gr",
    "Heights": "Hts",
    "Highway": "Hwy",
    "Hill": "Hl",
    "Inlet": "Inlet",
    "Junction": "Jct",
    "Key": "Key",
    "Landing": "Lndg",
    "Lane": "Ln",
    "Line": "Line",
    "Loop": "Loop",
    "Mall": "Mall",
    "Mead": "Mead",
    "Mews": "Mews",
    "Mile": "Mile",
    "Motu": "Motu",
    "Oaks": "Oaks",
    "Paku": "Paku",
    "Parade": "Pde",
    "Park": "Pk",
    "Parkway": "Pkwy",
    "Passage": "Psge",
    "Place": "Pl",
    "Plaza": "Plz",
    "Point": "Pt",
    "Promenade": "Prom",
    "Quadrant": "Qdrt|Qd",
    "Quay": "Qy",
    "Ridge": "Rdge",
    "Rise": "Rise",
    "River": "Riv",
    "Roadway": "Rdwy",
    "Road": "Rd",
    "Row": "Row",
    "Square": "Sq",
    "Steps": "Stps",
    "Strand": "Strd",
    "Street": "St",
    "Track": "Trk",
    "Terrace": "Tce",
    "Vale": "Vale",
    "Valley": "Vly",
    "View": "Vw",
    "Views": "Vws",
    "Vista": "Vis",
    "Village": "Vlg",
    "Walk": "Wlk",
    "Way": "Wy",
    "Wharf": "Whrf|Wrf"
}
WEIRD_STREETS = [
    "Airport",
    "Broadway",
    "Esplanade",
    "Highfields",
    "Motukiore",
    "Pukepoto",
    "Purerua",
    "Rakautapu",
    "Rangihoua",
    "Rangitane"
]
DIRECTIONS = {
    "North": "N",
    "East": "E",
    "South": "S",
    "West": "W",
    "Northeast": "NE",
    "Southeast": "SE",
    "Northwest": "NW",
    "Southwest": "SW",
    "Upper": "Upr",
    "Lower": "Lwr",
    "Central": "Ctrl",
    "Extension": "Ext",
}
UNITS = {
    "Apartment": "Apt",
    "Flat": "Flat",
    "Floor": "Fl",
    "Kiosk": "Ksk",
    "Level": "L",
    "Room": "Rm",
    "Shop": "Shp",
    "Suite": "Ste",
    "Unit": "Unit|Units",
    "Villa": "Villa"
}

STREETS_TYPES = fr"(?:{'|'.join(set(STREETS.keys()).union(set(STREETS.values())))})"
DIRECTION_TYPES = fr"(?:{'|'.join(set(DIRECTIONS.keys()).union(set(DIRECTIONS.values())))})"
UNIT_TYPES = fr"(?:{'|'.join(set(UNITS.keys()).union(set(UNITS.values())))})"
JOINERS = r"(?:\s*\b(?:and|to)\b\s*|\s*[\-\&\+\,]\s*)" # CAREFUL: [-&+,] are not words, so word boundaries don't work on them
STREET = fr"(?:\b[A-Za-z\s\-\']+ {STREETS_TYPES}(?: {DIRECTION_TYPES})?\b)"
WEIRD_STREET = fr"(?:\b(?:(?:The|Te|Ara|Motu|Rue) [A-Za-z\s\-\']+|Main (?:Road|Rd) [\w\s]+|(?:No\.?|Number) \d+ (?:Rd|Road|Line)|{'|'.join(WEIRD_STREETS)})\b)" # The Ridgeway, Te Wahapu, Broadway, etc
SH = fr"(?:\b(?:State Highway |SH)(\d+\w?)(?: {DIRECTION_TYPES})?\b)"
UNIT = fr"\b{UNIT_TYPES} \d*\w?(?:{JOINERS}\d*\w?)?\b|\b\d+\w?(?:{JOINERS}\d*\w?)?(?=\/)"
NUMBER = fr"(?:\d+[A-Za-z]{{0,2}}\b)" # Up to two letter suffix permitted
ADDITIONAL_NUMBER = fr"(?:{JOINERS}?\d*[A-Za-z]{{0,2}}\b)" # Up to two letter suffix permitted
STREET_NUMBER = re.compile(fr"({SH}|{WEIRD_STREET}|{STREET})[\.\, ]*({UNIT})?\/?({NUMBER})", flags = re.IGNORECASE) # Most of the addresses are in this weird pattern
NUMBER_STREET = re.compile(fr"({UNIT})?(?:\/|, )?({NUMBER}{ADDITIONAL_NUMBER}*) ({SH}|{WEIRD_STREET}|{STREET})", flags = re.IGNORECASE)
STREET_ONLY = re.compile(fr"({SH}|{WEIRD_STREET}|{STREET})", flags = re.IGNORECASE)
PARSE_STRATEGIES = {
    # "street-number": STREET_NUMBER,
    "number-street": NUMBER_STREET,
    "street-only": STREET_ONLY
}

def extract_all_addresses(raw, strategies = ["number-street", "street-only"]):
    unparsed = raw.strip()
    blocks = []
    # Extract address blocks
    while True:
        try:
            u, n, sn, st, sd, bs, unparsed = extract_address(unparsed, strategies)
            # Can't extract further - quit
            if not bs: break
            # Expand number range
            numbers = expand_number(n)
            if u and len(numbers) > 1:
                ne = "ambiguous unit/number"
                numbers = [(n[0], f"{n[1]}; {ne}" if n[1] else ne, n[2], n[3]) for n in numbers]
            elif len(numbers) == 0:
                ne = "no number"
                numbers = [(None, None, None, None)]
            # Expand unit range
            if u: units = expand_number(u, step_size = 1)
            else: units = [(None, None, None, None)]
            # Combine into block
            for un, ua, us, ue in units:
                u = f"{un or ''}{ua or ''}" or None
                for nn, na, ns, ne in numbers:
                    blocks.append((u, nn, na, sn, st, sd, bs, us, ue, ns, ne))
            unparsed = re.sub(r"^[\,\& ]+", "", unparsed)
            unparsed = re.sub(r"[\,\& ]+$", "", unparsed)
        except:
            print("Cannot parse:", raw)
            raise
    return blocks

def extract_address(raw, strategies):
    raw = raw.strip()
    for strategy in strategies:
        m = re.match(PARSE_STRATEGIES[strategy], raw)
        if m:
            if strategy == "street-number":
                u, n, street = [m[2], m[3], m[1]]
            elif strategy == "number-street":
                u, n, street = [m[1], m[2], m[3]]
            elif strategy == "street-only":
                u, n, street = [None, "", m[1]]
            n = clean_number(n)
            sn, st, sd = clean_street(street)
            unparsed = calc_remainder(raw, [u, n, street])
            return u, n, sn, st, sd, strategy, unparsed
    else:
        return None, None, None, None, None, None, raw

def expand_number(raw_range, step_size = None):
    m = None
    out = []
    unparsed = raw_range.upper()
    for i in range(0, 20):
        err = None
        # Remove previous match
        if m:
            unparsed = unparsed[:m.start()] + unparsed[m.end():]
        # Listed letters (only one letter permitted)
        m = re.search("(\d+)([A-Z]?)((?:[&, ]+[A-Z])+)", unparsed)
        if m:
            raw_letters = re.match("^[&, ]*(.*)[&, ]*$", m[3])[1]
            letters = [m[2]] + re.split("[&, ]+", raw_letters)
            out += [(m[1], l, "letter-list", err) for l in letters]
            continue
        # Letter range (only one letter permitted)
        m = re.search("(\d+)([A-Z])-(\d+)?([A-Z])", unparsed)
        if m:
            if m[1] != m[3]: err = "multiple numbers for letter-range"
            letters = range(ord(m[2]), ord(m[4]) + 1)
            out += [(m[1], chr(l), "letter-range", err) for l in letters]
            continue
        # Number range
        m = re.search("(\d+)-(\d+)", unparsed)
        if m:
            start_number = int(m[1])
            end_number = int(m[2])
            # If no step size is defined and range is odd-odd/even-even
            # treat as only covering one side of the street
            if step_size is None and start_number % 2 == end_number % 2:
                numbers = range(start_number, end_number + 1, 2)
                match_type = "number-range-skip"
            else:
                numbers = range(start_number, end_number + 1, 1)
                match_type = "number-range-full"
            for n in numbers:
                if n == start_number: match_position = "-start"
                elif n == end_number: match_position = "-end"
                else: match_position = "-middle"
                out.append((n, None, match_type + match_position, err))
            continue
        # Single address (up to 2 letters permitted)
        m = re.search("(\d+)([A-Z]{1,2})?", unparsed)
        if m:
            out.append((m[1], m[2], "number-single", err))
            continue
        # No more matches, exit
        break
    # Clean
    out = [(int(n), a, s, e) for n, a, s, e in out]
    return out

def clean_number(number):
    number = re.sub(r"\s*-\s*", "-", str(number))
    numbers = re.split(r"\s*(?:\band\b|[\&\+\,])\s*", number)
    numbers = [n.strip() for n in numbers]
    numbers.sort()
    return ", ".join(numbers).upper()

def clean_street(street):
    if not street: return
    unparsed = street.strip()
    out = {
        "street_name": None,
        "street_type": None,
        "street_direction": None
    }
    # Check for direction suffix first
    for dk,dv in DIRECTIONS.items():
        m = re.match(fr"(.+) ({dk}|{dv})$", unparsed, flags = re.IGNORECASE)
        if m:
            out["street_direction"] = dk
            unparsed = m[1]
            break
    # Normal street types
    for tk,tv in STREETS.items():
        m = re.match(fr"(.+) ({tk}|{tv})$", unparsed, flags = re.IGNORECASE)
        if m:
            out["street_type"] = tk
            out["street_name"] = m[1]
            return out.values()
    # Weird streets
    m = re.match(fr"({WEIRD_STREET})$", unparsed, flags = re.IGNORECASE)
    if m:
        out["street_type"] = ""
        out["street_name"] = m[1]
        return out.values()
    # State highways
    m = re.match(SH, unparsed, flags = re.IGNORECASE)
    if m:
        out["street_type"] = ""
        out["street_name"] = f"State Highway {m[1]}"
        return out.values()
    # Give up
    raise Exception(f"Unknown street_type for '{street}'!")

def get_number_range(n):
    if not n: return (None, None)
    m = re.findall("(\d+)", n)
    if not m: return (None, None)
    return (min(m), max(m))

def get_suburb_towncity(t):
    if not t: return (None, None)
    m = re.split(" *, *", t)
    if not m: return (None, None)
    m = [s.strip() for s in m]
    if len(m) == 1: return (m[0], m[0])
    return (m[0], m[1])

def calc_remainder(raw, parsed):
    for p in parsed:
        if p: raw = re.sub(fr"[, ]*\b{p}\b", "", raw, count = 1)
    raw = re.sub(fr"^[\-\/\, ]*", "", raw, count = 1)
    return raw.strip()

def clean_au_name(n):
    n = n.split("|")[0].strip()
    n = re.sub(r" E$", " East", n)
    n = re.sub(r" S$", " South", n)
    n = re.sub(r" N$", " North", n)
    n = re.sub(r" W$", " West", n)
    n = re.sub(r" Cen$", " Central", n)
    return n

def unabbreviate_street_type(abbrv_street_type):
    if not abbrv_street_type: return None
    for tk,tv in STREETS.items():
        m = re.match(fr"\b({tv})$", abbrv_street_type, flags = re.IGNORECASE)
        if m: return re.sub(tv, tk, abbrv_street_type)
    else:
        return abbrv_street_type
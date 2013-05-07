#include "linker.h"

Section* symtab_section;
Section* strtab_section;
Section* hash_section;
Section* dynsymtab_section;
Section* bss_section;

static int new_undef_sym = 0; /* Is there a new undefined sym since last new_undef_sym() */

typedef struct Sections { 
  size_t   size;
  size_t   allocated;
  Section** sec;
}Sections;

Sections* Secs;

/* realloc section and set its content to zero */
void section_realloc(Section *sec, unsigned long new_size)
{
  unsigned long size;
  unsigned char *data;
    
  size = sec->data_allocated;
  if (size == 0)
    size = 1;
  while (size < new_size)
    size = size * 2;
  data = realloc(sec->data, size);
  memset(data + sec->data_allocated, 0, size - sec->data_allocated);
  sec->data = data;
  sec->data_allocated = size;
}

/* elf symbol hashing function */
static unsigned long elf_hash(const unsigned char *name)
{
  unsigned long h = 0, g;
    
  while (*name) {
    h = (h << 4) + *name++;
    g = h & 0xf0000000;
    if (g)
      h ^= g >> 24;
    h &= ~g;
  }
  return h;
}

/* reserve at least 'size' bytes in section 'sec' from
   sec->data_offset. */
void *section_ptr_add(Section *sec, unsigned long size)
{
  unsigned long offset, offset1;

  offset = sec->data_offset;
  offset1 = offset + size;
  if (offset1 > sec->data_allocated)
    section_realloc(sec, offset1);
  sec->data_offset = offset1;
  return sec->data + offset;
}

static void *load_data(int fd, unsigned long file_offset, unsigned long size)
{
  void *data;

  data = malloc(size);
  lseek(fd, file_offset, SEEK_SET);
  read(fd, data, size);
  return data;
}

int put_elf_str(Section *s, const char *sym)
{
  int offset, len;
  char *ptr;

  len = strlen(sym) + 1;
  offset = s->data_offset;
  ptr = section_ptr_add(s, len);
  memcpy(ptr, sym, len);
  return offset;
}

/* rebuild hash table of section s */
/* NOTE: we do factorize the hash table code to go faster */
static void rebuild_hash(Section *s, unsigned int nb_buckets)
{
  ElfW(Sym) *sym;
  int *ptr, *hash, nb_syms, sym_index, h;
  char *strtab;

  strtab = s->link->data;
  nb_syms = s->data_offset / sizeof(ElfW(Sym));

  s->hash->data_offset = 0;
  ptr = section_ptr_add(s->hash, (2 + nb_buckets + nb_syms) * sizeof(int));
  ptr[0] = nb_buckets;
  ptr[1] = nb_syms;
  ptr += 2;
  hash = ptr;
  memset(hash, 0, (nb_buckets + 1) * sizeof(int));
  ptr += nb_buckets + 1;

  sym = (ElfW(Sym) *)s->data + 1;
  for(sym_index = 1; sym_index < nb_syms; sym_index++) {
    if (ELFW(ST_BIND)(sym->st_info) != STB_LOCAL) {
      h = elf_hash(strtab + sym->st_name) % nb_buckets;
      *ptr = hash[h];
      hash[h] = sym_index;
    } else {
      *ptr = 0;
    }
    ptr++;
    sym++;
  }
}

Section *new_section(Sections* ss, const char *name, int sh_type, int sh_flags)
{
  Section *sec;

  sec = malloc(sizeof(Section) + strlen(name));
  memset(sec, 0, sizeof(Section) + strlen(name));

  strcpy(sec->name, name);
  sec->sh_type = sh_type;
  sec->sh_flags = sh_flags;
  switch(sh_type) {
  case SHT_HASH:
  case SHT_REL:
  case SHT_RELA:
  case SHT_DYNSYM:
  case SHT_SYMTAB:
  case SHT_DYNAMIC:
    sec->sh_addralign = 4;
    break;
  case SHT_STRTAB:
    sec->sh_addralign = 1;
    break;
  default:
    sec->sh_addralign = 32; /* default conservative alignment */
    break;
  }

  if (ss->size >= ss->allocated) {
    int i = ss->allocated + 10;
    ss->sec = realloc(ss->sec, i * sizeof(Section *));
    ss->allocated = i;
    
  }
  sec->sh_num = ss->size;
  ss->sec[ss->size] = sec;
  ss->size += 1;

  return sec;
}

/* find global ELF symbol 'name' and return its index. Return 0 if not
   found. */
int find_elf_sym(Section *s, const char *name)
{
  ElfW(Sym) *sym;
  Section *hs;
  int nbuckets, sym_index, h;
  const char *name1;
    
  hs = s->hash;
  if (!hs)
    return 0;
  nbuckets = ((int *)hs->data)[0];
  h = elf_hash(name) % nbuckets;
  sym_index = ((int *)hs->data)[2 + h];
  while (sym_index != 0) {
    sym = &((ElfW(Sym) *)s->data)[sym_index];
    name1 = s->link->data + sym->st_name;
    if (!strcmp(name, name1))
      return sym_index;
    sym_index = ((int *)hs->data)[2 + nbuckets + sym_index];
  }
  return 0;
}

/* return the symbol number */
int put_elf_sym(Section *s, addr_t value, unsigned long size,
                int info, int other, int shndx, const char *name)
{
  int name_offset, sym_index;
  int nbuckets, h;
  ElfW(Sym) *sym;
  Section *hs;
    
  sym = section_ptr_add(s, sizeof(ElfW(Sym)));
  if (name)
    name_offset = put_elf_str(s->link, name);
  else
    name_offset = 0;
  /* XXX: endianness */
  sym->st_name = name_offset;
  sym->st_value = value;
  sym->st_size = size;
  sym->st_info = info;
  sym->st_other = other;
  sym->st_shndx = shndx;
  sym_index = sym - (ElfW(Sym) *)s->data;
  hs = s->hash;
  if (hs) {
    int *ptr, *base;
    ptr = section_ptr_add(hs, sizeof(int));
    base = (int *)hs->data;
    /* only add global or weak symbols */
    if (ELFW(ST_BIND)(info) != STB_LOCAL) {
      /* add another hashing entry */
      nbuckets = base[0];
      h = elf_hash(name) % nbuckets;
      *ptr = base[2 + h];
      base[2 + h] = sym_index;
      base[1]++;
      /* we resize the hash table */
      hs->nb_hashed_syms++;
      if (hs->nb_hashed_syms > 2 * nbuckets) {
        rebuild_hash(s, 2 * nbuckets);
      }
    } else {
      *ptr = 0;
      base[1]++;
    }
  }
  return sym_index;
}

#define SHN_UNDEF       0               /* Undefined section */

int add_elf_sym(Section *s, addr_t value, unsigned long size,
                int info, int other, int sh_num, const char* name)
{
  ElfW(Sym) *esym;
  int sym_bind, sym_index, sym_type, esym_bind;
  unsigned char sym_vis, esym_vis, new_vis;

  sym_bind = ELFW(ST_BIND)(info);
  sym_type = ELFW(ST_TYPE)(info);
  sym_vis = ELFW(ST_VISIBILITY)(other);
        
  if (sym_bind != STB_LOCAL) {
    /* we search global or weak symbols */
    sym_index = find_elf_sym(s, name);
    if (!sym_index)
      goto do_def;
    esym = &((ElfW(Sym) *)s->data)[sym_index];
    if (esym->st_shndx != SHN_UNDEF) {
      esym_bind = ELFW(ST_BIND)(esym->st_info);
      /* propagate the most constraining visibility */
      /* STV_DEFAULT(0)<STV_PROTECTED(3)<STV_HIDDEN(2)<STV_INTERNAL(1) */
      esym_vis = ELFW(ST_VISIBILITY)(esym->st_other);
      if (esym_vis == STV_DEFAULT) {
        new_vis = sym_vis;
      } else if (sym_vis == STV_DEFAULT) {
        new_vis = esym_vis;
      } else {
        new_vis = (esym_vis < sym_vis) ? esym_vis : sym_vis;
      }
      esym->st_other = (esym->st_other & ~ELFW(ST_VISIBILITY)(-1))
        | new_vis;
      other = esym->st_other; /* in case we have to patch esym */
      if (sh_num == SHN_UNDEF) {
        /* ignore adding of undefined symbol if the
           corresponding symbol is already defined */
      } else if (sym_bind == STB_GLOBAL && esym_bind == STB_WEAK) {
        /* global overrides weak, so patch */
        goto do_patch;
      } else if (sym_bind == STB_WEAK && esym_bind == STB_GLOBAL) {
        /* weak is ignored if already global */
      } else if (sym_bind == STB_WEAK && esym_bind == STB_WEAK) {
        /* keep first-found weak definition, ignore subsequents */
      } else if (sym_vis == STV_HIDDEN || sym_vis == STV_INTERNAL) {
        /* ignore hidden symbols after */
      } else if (esym->st_shndx == SHN_COMMON
                 && (sh_num < SHN_LORESERVE || sh_num == SHN_COMMON)) {
        /* gr: Happens with 'tcc ... -static tcctest.c' on e.g. Ubuntu 6.01
           No idea if this is the correct solution ... */
        goto do_patch;
      } else if (s == dynsymtab_section) {
        /* we accept that two DLL define the same symbol */
      } else {
        assert(0 && "'%s' defined twice");
      }
    } else {
    do_patch:
      esym->st_info = ELFW(ST_INFO)(sym_bind, sym_type);
      esym->st_shndx = sh_num;
      new_undef_sym = 1;
      esym->st_value = value;
      esym->st_size = size;
      esym->st_other = other;
    }
  } else {
  do_def:
    sym_index = put_elf_sym(s, value, size, 
                            ELFW(ST_INFO)(sym_bind, sym_type), other, 
                            sh_num, name);
  }
  return sym_index;

}

int load_object_file(int fd, unsigned long file_offset) {

  ElfW(Ehdr) ehdr;
  ElfW(Shdr) *shdr, *sh;
  ElfW(Sym) *sym, *symtab;
  
  SectionMergeInfo *sm_table, *sm;
  unsigned char *strsec, *strtab;
  int i, j, nb_syms, offset;
  int size, sym_index;
  char *sh_name, *name;
  Section *s;
  int *old_to_new_syms;
  int stab_index;
  int stabstr_index;
  int offseti;
  ElfW_Rel *rel, *rel_end;

  if (read(fd, &ehdr, sizeof(ehdr)) != sizeof(ehdr))
    assert(0);

  if (ehdr.e_ident[0] != ELFMAG0 ||
      ehdr.e_ident[1] != ELFMAG1 ||
      ehdr.e_ident[2] != ELFMAG2 ||
      ehdr.e_ident[3] != ELFMAG3)
    assert(0);

  /* test if object file */
  if (ehdr.e_type != ET_REL)
    assert(0);
  /* test CPU specific stuff */
  if (ehdr.e_ident[5] != ELFDATA2LSB ||
      ehdr.e_machine != TARGET) {
    assert(0);
    return -1;
  }
  
  /* read sections */
  shdr = load_data(fd, file_offset + ehdr.e_shoff, 
                   sizeof(ElfW(Shdr)) * ehdr.e_shnum);
  
  sm_table = malloc(sizeof(SectionMergeInfo) * ehdr.e_shnum);  

  /* load section names */
  sh = &shdr[ehdr.e_shstrndx];
  strsec = load_data(fd, file_offset + sh->sh_offset, sh->sh_size);

  symtab = NULL;
  for(i = 1; i < ehdr.e_shnum; i++) {
    sh = &shdr[i];
    if (sh->sh_type == SHT_SYMTAB) {
      if (symtab) {
        assert(0);
      }
      nb_syms = sh->sh_size / sizeof(ElfW(Sym));
      symtab = load_data(fd, file_offset + sh->sh_offset, sh->sh_size);
      sm_table[i].s = symtab_section;

      /* now load strtab */
      sh = &shdr[sh->sh_link];
      strtab = load_data(fd, file_offset + sh->sh_offset, sh->sh_size);
    }
  }

  /* now examine each section and try to merge its content with the
     ones in memory */
  for(i = 1; i < ehdr.e_shnum; i++) {
    /* no need to examine section name strtab */
    if (i == ehdr.e_shstrndx)
      continue;
    sh = &shdr[i];
    sh_name = strsec + sh->sh_name;
    /* ignore sections types we do not handle */
    if (sh->sh_type != SHT_PROGBITS &&
        sh->sh_type != SHT_RELX && 
        sh->sh_type != SHT_NOBITS && 
        sh->sh_type != SHT_PREINIT_ARRAY &&
        sh->sh_type != SHT_INIT_ARRAY &&
        sh->sh_type != SHT_FINI_ARRAY &&
        strcmp(sh_name, ".stabstr")
        )
      continue;
    if (sh->sh_addralign < 1)
      sh->sh_addralign = 1;
    /* find corresponding section, if any */
    for(j = 1; j < Secs->size;j++) {
      s = Secs->sec[j];
      if (!strcmp(s->name, sh_name)) {
        if (!strncmp(sh_name, ".gnu.linkonce", 
                     sizeof(".gnu.linkonce") - 1)) {
          /* if a 'linkonce' section is already present, we
             do not add it again. It is a little tricky as
             symbols can still be defined in
             it. */
          sm_table[i].link_once = 1;
          goto next;
        } else {
          goto found;
        }
      }
    }
    /* not found: create new section */
    s = new_section(Secs, sh_name, sh->sh_type, sh->sh_flags);
    /* take as much info as possible from the section. sh_link and
       sh_info will be updated later */
    s->sh_addralign = sh->sh_addralign;
    s->sh_entsize = sh->sh_entsize;
    sm_table[i].new_section = 1;
  found:
    if (sh->sh_type != s->sh_type) {
      assert(0);
    }

    /* align start of section */
    offset = s->data_offset;

    if (0 == strcmp(sh_name, ".stab")) {
      stab_index = i;
      assert(0 && "what's this");
      goto no_align;
    }
    if (0 == strcmp(sh_name, ".stabstr")) {
      stabstr_index = i;
      goto no_align;
    }

    size = sh->sh_addralign - 1;
    offset = (offset + size) & ~size;
    if (sh->sh_addralign > s->sh_addralign)
      s->sh_addralign = sh->sh_addralign;
    s->data_offset = offset;
  no_align:
    sm_table[i].offset = offset;
    sm_table[i].s = s;
    /* concatenate sections */
    size = sh->sh_size;
    if (sh->sh_type != SHT_NOBITS) {
      unsigned char *ptr;
      lseek(fd, file_offset + sh->sh_offset, SEEK_SET);
      ptr = section_ptr_add(s, size);
      read(fd, ptr, size);
    } else {
      s->data_offset += size;
    }
  next: ;
  }
  
  /* //gr relocate stab strings */
  /* if (stab_index && stabstr_index) { */
  /*   Stab_Sym *a, *b; */
  /*   unsigned o; */
  /*   s = sm_table[stab_index].s; */
  /*   a = (Stab_Sym *)(s->data + sm_table[stab_index].offset); */
  /*   b = (Stab_Sym *)(s->data + s->data_offset); */
  /*   o = sm_table[stabstr_index].offset; */
  /*   while (a < b)  */
  /*     a->n_strx += o, a++; */
  /* } */
  
  /* second short pass to update sh_link and sh_info fields of new
     sections */
  for(i = 1; i < ehdr.e_shnum; i++) {
    s = sm_table[i].s;
    if (!s || !sm_table[i].new_section)
      continue;
    sh = &shdr[i];
    if (sh->sh_link > 0)
      s->link = sm_table[sh->sh_link].s;
    if (sh->sh_type == SHT_RELX) {
      s->sh_info = sm_table[sh->sh_info].s->sh_num;
      /* update backward link */
      Secs->sec[s->sh_info]->reloc = s;
    }
  }
  sm = sm_table;
  
  /* resolve symbols */
  old_to_new_syms = malloc(nb_syms * sizeof(int));

  sym = symtab + 1;
  for(i = 1; i < nb_syms; i++, sym++) {
    if (sym->st_shndx != SHN_UNDEF &&
        sym->st_shndx < SHN_LORESERVE) {
      sm = &sm_table[sym->st_shndx];
      if (sm->link_once) {
        /* if a symbol is in a link once section, we use the
           already defined symbol. It is very important to get
           correct relocations */
        if (ELFW(ST_BIND)(sym->st_info) != STB_LOCAL) {
          name = strtab + sym->st_name;
          sym_index = find_elf_sym(symtab_section, name);
          if (sym_index)
            old_to_new_syms[i] = sym_index;
        }
        continue;
      }
      /* if no corresponding section added, no need to add symbol */
      if (!sm->s)
        continue;
      /* convert section number */
      sym->st_shndx = sm->s->sh_num;
      /* offset value */
      sym->st_value += sm->offset;
    }
    /* add symbol */
    name = strtab + sym->st_name;
    sym_index = add_elf_sym(symtab_section, sym->st_value, sym->st_size, 
                            sym->st_info, sym->st_other, 
                            sym->st_shndx, name);
    old_to_new_syms[i] = sym_index;
  }
  
  /* third pass to patch relocation entries */
  for(i = 1; i < ehdr.e_shnum; i++) {
    s = sm_table[i].s;
    if (!s)
      continue;
    sh = &shdr[i];
    offset = sm_table[i].offset;
    switch(s->sh_type) {
    case SHT_RELX:
      /* take relocation offset information */
      offseti = sm_table[sh->sh_info].offset;
      rel_end = (ElfW_Rel *)(s->data + s->data_offset);
      for(rel = (ElfW_Rel *)(s->data + offset);
          rel < rel_end;
          rel++) {
        int type;
        unsigned sym_index;
        /* convert symbol index */
        type = ELFW(R_TYPE)(rel->r_info);
        sym_index = ELFW(R_SYM)(rel->r_info);
        /* NOTE: only one symtab assumed */
        if (sym_index >= nb_syms)
          goto invalid_reloc;
        sym_index = old_to_new_syms[sym_index];
        /* ignore link_once in rel section. */
        if (!sym_index && !sm->link_once
            ) {
        invalid_reloc:
          assert(0);
        }
        rel->r_info = ELFW(R_INFO)(sym_index, type);
        /* offset the relocation offset */
        rel->r_offset += offseti;
      }
      break;
    default:
      break;
    }
  }


}

int main(int argc, char ** argv) {

  int i = 0;
  Secs = malloc(sizeof(struct Sections)); 
  Secs->allocated = 0;
  Secs->size = 0;
  symtab_section = new_section(Secs, ".symtab", SHT_SYMTAB, SHF_ALLOC);
  strtab_section = new_section(Secs, ".strtab", SHT_STRTAB, SHF_ALLOC);
  hash_section = new_section(Secs, ".hash", SHT_HASH, SHF_ALLOC);
  bss_section = new_section(Secs, ".bss", SHT_NOBITS, SHF_ALLOC);
  symtab_section->link = strtab_section;
  symtab_section->hash = hash_section;
  
  int *data = section_ptr_add(hash_section, sizeof(int) * 3);
  data[0] = 1;

  add_elf_sym(symtab_section, 0, 0, 0, 0, 
              0, "");
  
  for (i = 1; i < argc; i++) {
    int fd = open(argv[i], O_RDONLY);
    load_object_file(fd, 0);
  }
  printf("section size = %lu\n", Secs->size);
}

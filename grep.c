/*
 * grep re-implementation based on ed's g/re/p
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#define	FNSIZE	64
#define	LBSIZE	512
#define	ESIZE	128
#define	GBSIZE	256
#define	NBRA	5
#define	KSIZE	9

#define	CBRA	1
#define	CCHR	2
#define	CDOT	4
#define	CCL	6
#define	NCCL	8
#define	CDOL	10
#undef CEOF
#define	CEOF	11
#define	CKET	12
#define	CBACK	14

#define	STAR	01

char	Q[]	= "";
char	T[]	= "TMP";
#define	READ	0
#define	WRITE	1

#ifdef ISDEBUG
#define DEBUG(fmt, args...)    fprintf(stderr, fmt, ## args)
#else
#define DEBUG(fmt, args...)    /* Don't do anything in release builds */
#endif

int main(int argc, char **argv)
{
	register char *p1, *p2;
    void (*oldintr)(int);

	int i = 1;
    for (; i < argc && argv[i][0] == '-'; ++i)
    {
		DEBUG("%d: %s (argv[i][0]=%c))\n", i, argv[i], argv[i][0]);
	}

    char filename[FNSIZE];
    char expression[ESIZE+4];

    // Reading the search expression
	if (i < argc) {
		p1 = argv[i];
		p2 = expression;
		while (*p2++ = *p1++)
			;
        i += 1;
	}
	DEBUG("expression: %s\n", expression);
    char expbuf[ESIZE+4];
    compile(expression, expbuf);
    DEBUG("compiled expression: %s\n", expbuf);

    // File path to process
	if (i < argc) {
		p1 = argv[i];
		p2 = filename;
		while (*p2++ = *p1++)
			;
	}
	DEBUG("filename: %s\n", filename);
    DEBUG("\n");

    FILE * fp;
    char * line = NULL;
    size_t len = 0;
    ssize_t read;
    fp = fopen(filename, "r");
    if (fp == NULL)
        exit(EXIT_FAILURE);
    while ((read = getline(&line, &len, fp)) != -1) {
        int check = execute(line, expbuf);
        DEBUG("Retrieved line of length %zu, checked: %i: %s", read, check, line);
        DEBUG("\n");
        if (check) printf("%s", line);
    }
    fclose(fp);
    if (line)
        free(line);
    exit(EXIT_SUCCESS);
}

error(s)
char *s;
{
    printf("Error: %s", s);
}

compile(char * expression, char * out_expbuf)
{
	register eof, c;
	register char *ep;
	char *lastep;
	char bracket[NBRA], *bracketp;
	int cclcnt;

    char * ptr = expression;
	ep = out_expbuf;
	eof = '\0';
	bracketp = bracket;
	int circfl = 0;
	int nbra = 0;
	if (c=='^') {
		c = *ptr++;
		circfl++;
	}
	int peekc = c;
	lastep = 0;

    for (;;) {
        c = *ptr++;
		if (c==eof) {
			if (bracketp != bracket)
				goto cerror;
			*ep++ = CEOF;
			return;
		}
		if (c!='*')
			lastep = ep;
		switch (c) {

		case '\\':
			if ((c = *ptr++)=='(') {
				if (nbra >= NBRA)
					goto cerror;
				*bracketp++ = nbra;
				*ep++ = CBRA;
				*ep++ = nbra++;
				continue;
			}
			if (c == ')') {
				if (bracketp <= bracket)
					goto cerror;
				*ep++ = CKET;
				*ep++ = *--bracketp;
				continue;
			}
			if (c>='1' && c<'1'+NBRA) {
				*ep++ = CBACK;
				*ep++ = c-'1';
				continue;
			}
			*ep++ = CCHR;
			if (c=='\n')
				goto cerror;
			*ep++ = c;
			continue;

		case '.':
			*ep++ = CDOT;
			continue;

		case '\n':
			goto cerror;

		case '*':
			if (lastep==0 || *lastep==CBRA || *lastep==CKET)
				goto defchar;
			*lastep |= STAR;
			continue;

		case '$':
			if ((peekc=*ptr++) != eof)
				goto defchar;
			*ep++ = CDOL;
			continue;

		case '[':
			*ep++ = CCL;
			*ep++ = 0;
			cclcnt = 1;
			if ((c=*ptr++) == '^') {
				c = *ptr++;
				ep[-2] = NCCL;
			}
			do {
				if (c=='\n')
					goto cerror;
				if (c=='-' && ep[-1]!=0) {
					if ((c=*ptr++)==']') {
						*ep++ = '-';
						cclcnt++;
						break;
					}
					while (ep[-1]<c) {
						*ep = ep[-1]+1;
						ep++;
						cclcnt++;
						if (ep>=&out_expbuf[ESIZE])
							goto cerror;
					}
				}
				*ep++ = c;
				cclcnt++;
				if (ep >= &out_expbuf[ESIZE])
					goto cerror;
			} while ((c = *ptr++) != ']');
			lastep[1] = cclcnt;
			continue;

		defchar:
		default:
			*ep++ = CCHR;
			*ep++ = c;
		}
	}
   cerror:
	out_expbuf[0] = 0;
	nbra = 0;
	error(Q);
}

char	*braslist[NBRA];
char	*braelist[NBRA];
int	circfl;
char	*loc1;
char	*loc2;
char	*locs;

execute(char * line, char * expbuf)
{
	char *p1, *p2, c;

	for (c=0; c<NBRA; c++) {
		braslist[c] = 0;
		braelist[c] = 0;
	}
    p1 = line;
	p2 = expbuf;
    DEBUG("execute: p1=%s, p2=%s\n", p1, p2);
    DEBUG("circfl=%d, CCHR=%d, *p2=%d\n", circfl, CCHR, *p2);
	if (circfl) {
		loc1 = p1;
		return(advance(p1, p2));
	}
	/* fast check for first character */
	if (*p2==CCHR) {
		c = p2[1];
        int roundn = 0;
		do {
            DEBUG("round %d: c=%d '%c', *p1=%d '%c', *p2=%d '%c'\n", roundn, c, c, *p1, *p1, *p2, *p2);
            roundn += 1;
			if (*p1!=c)
				continue;
			if (advance(p1, p2)) {
				loc1 = p1;
				return(1);
			}
		} while (*p1++);
		return(0);
	}
	/* regular algorithm */
	do {
		if (advance(p1, p2)) {
			loc1 = p1;
			return(1);
		}
	} while (*p1++);
	return(0);
}

advance(char *lp, char *ep)
{
	register char *curlp;
	int i;
    
    DEBUG("advance: lp=%c, ep[0]=%c, ep[1]=%c\n", *lp, ep[0], ep[1]);

    int j = 0;
	for (;;) {
        switch (ep[j++]) {

        case CCHR:
            DEBUG("CCHR: j=%d, lp=%d %c, ep[j]=%d %c\n", j, *lp, *lp, ep[j], ep[j]);
            if (ep[j++] == *lp++)
                continue;
            return(0);

        case CDOT:
            DEBUG("CDOT: lp=%c, ep[j]=%c\n", *lp, ep[j]);
            if (*lp++)
                continue;
            return(0);

        case CDOL:
            DEBUG("CDOL: lp=%c, ep[j]=%c\n", *lp, ep[j]);
            if (*lp==0)
                continue;
            return(0);

        case CEOF:
            DEBUG("CEOF: lp=%c, ep[j]=%c\n", *lp, ep[j]);
            loc2 = lp;
            return(1);

        case CCL:
            DEBUG("CCL: lp=%c, ep[j]=%c\n", *lp, ep[j]);
            if (cclass(ep+j, *lp++, 1)) {
                j += ep[j];
                continue;
            }
            return(0);

        case NCCL:
            DEBUG("NCCL: lp=%c, ep=%c\n", *lp, ep[j]);
            if (cclass(ep+j, *lp++, 0)) {
                j += ep[j];
                continue;
            }
            return(0);

        case CBRA:
            DEBUG("CBRA: lp=%c, ep[j]=%c\n", *lp, ep[j]);
            braslist[ep[j++]] = lp;
            continue;

        case CKET:
            DEBUG("CKET: lp=%c, ep[j]=%c\n", *lp, ep[j]);
            braelist[ep[j++]] = lp;
            continue;

        case CBACK:
            DEBUG("CBACK: lp=%c, ep[j]=%c\n", *lp, ep[j]);
            if (braelist[i = ep[j++]]==0)
                error(Q);
            if (backref(i, lp)) {
                lp += braelist[i] - braslist[i];
                continue;
            }
            return(0);

        case CBACK|STAR:
            if (braelist[i = ep[j++]] == 0)
                error(Q);
            curlp = lp;
            while (backref(i, lp))
                lp += braelist[i] - braslist[i];
            while (lp >= curlp) {
                if (advance(lp, ep+j))
                    return(1);
                lp -= braelist[i] - braslist[i];
            }
            continue;

        case CDOT|STAR:
            curlp = lp;
            while (*lp++)
                ;
            goto star;

        case CCHR|STAR:
            curlp = lp;
            while (*lp++ == ep[j])
                ;
            j++;
            goto star;

        case CCL|STAR:
        case NCCL|STAR:
            curlp = lp;
            while (cclass(ep+j, *lp++, ep[j-1]==(CCL|STAR)))
                ;
            j += ep[j];
            goto star;

        star:
            do {
                lp--;
                if (lp==locs)
                    break;
                if (advance(lp, ep+j))
                    return(1);
            } while (lp > curlp);
            return(0);

        default:
            DEBUG("default: lp=%c, ep[j]=%c\n", *lp, ep[j]);
            error(Q);
        }
    }
}

backref(i, lp)
register i;
register char *lp;
{
	register char *bp;

	bp = braslist[i];
	while (*bp++ == *lp++)
		if (bp >= braelist[i])
			return(1);
	return(0);
}

cclass(set, c, af)
register char *set, c;
{
	register n;

	if (c==0)
		return(0);
	n = *set++;
	while (--n)
		if (*set++ == c)
			return(af);
	return(!af);
}


crblock(permp, buf, nchar, startn)
char *permp;
char *buf;
long startn;
{
	register char *p1;
	int n1;
	int n2;
	register char *t1, *t2, *t3;

	t1 = permp;
	t2 = &permp[256];
	t3 = &permp[512];

	n1 = startn&0377;
	n2 = (startn>>8)&0377;
	p1 = buf;
	while(nchar--) {
		*p1 = t2[(t3[(t1[(*p1+n1)&0377]+n2)&0377]-n2)&0377]-n1;
		n1++;
		if(n1==256){
			n1 = 0;
			n2++;
			if(n2==256) n2 = 0;
		}
		p1++;
	}
}

---
title: Employ.ed Internship: week one
date: 2014-06-06
---

So that's the first week of my internship almost over, out of a planned eight. I'm here in the IT department of the university's [School of Engineering](http://eng.ed.ac.uk/), officially employed as an "API Systems Developer intern" according to the namebadge given to me on the [Employ.ed](http://www.ed.ac.uk/schools-departments/careers/edinburgh-award/employ-ed/overview) induction day.

I've been employed to with a few different project goals in mind; investigating and using APIs in order to integrate existing systems and automate business processes, or refactoring current services into APIs in order to make them more easily accessible and reusable across the school and university.

## Projects

I've spent most of my time this week in my new corner desk getting set up with the internal university systems, and investigating the possibility of a few putative summer projects suggested by my manager, Colin Higgs:

1. Tying data from Learn into the university's BI Suite as a new data source, in order for stakeholders (Personal Tutors/school administrators/etc.) to generate more comprehensive data reports than Learn will currently let them.

2. Automating various pieces of manual office-work for the <acronym title="Engineering Teaching Office">ETO</acronym> and school IT department, such as
    a. generating custom course timetables
    b. synchronising PC details to IS's EdLAN database

3. Building an API for Euclid (the current "golden master" for teaching data) for the school IT department to integrate directly into their systems, instead of using the current overnight dumps.

These smaller summer projects are all about letting the school IT team integrate and build on the data from other systems/departments across the university, rather than replicating data (and the effort of maintaining and synchronising it).

## My Go-To BHAG

My Big Hairy Audacious Goal for this internship is to be able to build some sort of MVP demonstrating the power and usefulness of integrating data from the original sources, instead of replicating it across departments. For example, an application for engineering <acronym title="Personal Tutor">PT</acronym> to pull up all their information on a tutee -- accessing

- Learn for course marks,
- Euclid for enrolment, and
- T@Ed for timetables.

## Groupthink

Meanwhile, Eng-IT's stretch goal for the summer is to build an abstraction layer atop the university's various group authorisation systems, bundling them *all* up into one interface for

- LDAP organisations,
- *nix groups,
- student/teacher enrolment,
- etc.

All this is happening under the codename "Groupthink".

My projects may or may not contribute to this as the summer proceeds, but I will certainly find it useful over the course of my work here, if it gets completed before my eight weeks are up!

## This Week's Work

Writing this on Friday afternoon, my first week has been mostly comprised of getting accounts on various university systems, and attempting to

1. investigate the Blackboard Learn API, and
2. investigate what the BI Suite will accept as a data source

for Project Number 1 (above).

However, due to the Learn's developer docs being hidden "[Behind The Blackboard](http://behind.blackboard.com/)", we're currently waiting on responses from our "local Blackboard administrator" in order to access them, as well as the Learn testing/dev environments.

For now I'm poking my `psql` around the school's internal managed-machine Postgresql tables, in order to automatically sync them to IS's central EdLAN registry, without which the
Windows supported desktops cannot be built.

This currently happens through manual invocation of a Perl script, which sends the appropriate data through some web forms (the only interface currently provided by IS).

So I'm now brushing up on my SQL-foo and finding out how to trigger events from Postgres, in order to automate this!

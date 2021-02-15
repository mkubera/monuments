// Docs on event and context https://www.netlify.com/docs/functions/#the-handler-method
exports.handler = async (event) => {
  const { DB_API_KEY } = process.env;
  var Airtable = require("airtable");
  var base = new Airtable({
    apiKey: DB_API_KEY,
  }).base("appUOSxxv2dDfXs4t");

  base("4")
    .select({
      pageSize: 100,
      maxRecords: 1,
      view: "Grid view",
    })
    .eachPage(
      function page(records, fetchNextPage) {
        // This function (`page`) will get called for each page of records.

        records.forEach(function (record) {
          console.log("Retrieved", record.get("Name"));
        });

        return {
          statusCode: 200,
          body: JSON.stringify(records),
        };

        // To fetch the next page of records, call `fetchNextPage`.
        // If there are more records, `page` will get called again.
        // If there are no more records, `done` will get called.
        fetchNextPage();
      },
      function done(err) {
        if (err) {
          console.error(err);
          return;
        }
      }
    );
};
